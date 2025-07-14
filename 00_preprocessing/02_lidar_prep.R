##### ----- Raw Point Cloud data Processing ----- #####
#### Author: Leah E. Staub
#### Creation Date: 07/06/2025
#### Update Date: 07/14/2025
#### Purpose: This script takes raw lidar point cloud data and classifies & filters it to ground, vegetation, and surface points. It does this by retiling the point cloud data for efficient processing.   

# Set up Environment ----
library(pacman)
p_load(mapview, sf, ggplot2, mapview, lidR, terra, tidyterra, fs, archive, tools, tidyverse)


#Locations of LGF data
# Harf2013 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ34/clipped_chunk_1.laz")
# Harf2020 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Harford_2020_BLK1/clipped_chunk_1.laz")
# Balt2015 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ30/clipped_chunk_1.laz")

#Locations of Patap files
# How2011a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_2.laz")
# How2011b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_3.laz")
# How2018 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/clipped_chunk_2.laz")
# Balt2015 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ31/clipped_chunk_2.laz")

# #Locations of Patux files
# How2011a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_2.laz")
# How2011b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_3.laz")
# How2018 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/How_2018_BLK_1/clipped_chunk_3.laz")

# #Locations of Sen files
# Mont2020a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Montgomery_2020_BLK2/clipped_chunk_4.laz")
# Mont2020b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Montgomery_2020_BLK3/clipped_chunk_4.laz")
# Mont2018a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Mont_2018_BLK2/clipped_chunk_4.laz")
# Mont2018b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Mont_2018_BLK4/clipped_chunk_4.laz")






#### Potential New workflow -----
process_and_save_tile <- function(chunk, output_dir, buffer, remove_duplicates = TRUE) {
  las <- readLAS(chunk)
  
  if (is.null(las) || npoints(las) == 0) {
    message("⚠️ Empty tile found, skipping")
    return(NULL)
  }
  
  message("📦 Processing tile with ", npoints(las), " points")
  
  # Optionally remove exact duplicate points
  if (remove_duplicates) {
    las@data <- distinct(las@data)
  }
  
  # Classify ground
  las <- classify_ground(las, algorithm = csf())
  
  # Filter to keep ground, vegetation, and surface (last returns)
  las <- lasfilter(las, Classification %in% 2:5 | ReturnNumber == NumberOfReturns)
  
  # Trim buffer — keep only the core area
  core <- raster::extent(chunk)  # extent from chunk (includes buffer)
  core@xmin <- core@xmin + buffer
  core@xmax <- core@xmax - buffer
  core@ymin <- core@ymin + buffer
  core@ymax <- core@ymax - buffer
  las <- lasclipRectangle(las, core@xmin, core@ymin, core@xmax, core@ymax)
  
  # Output filename based on chunk's core origin
  tile_name <- paste0("tile_", chunk@header$X[1], "_", chunk@header$Y[1], ".laz")
  out_file <- file.path(output_dir, tile_name)
  
  writeLAS(las, out_file)
  message("✅ Processed tile written to: ", out_file)
  
  return(out_file)
}

run_memory_efficient_workflow <- function(input_dirs, processed_dir, tile_size = 1000, buffer = 30, crs_target = NULL) {
  laz_files <- unlist(lapply(input_dirs, function(dir) {
    dir_ls(dir, regexp = "\\.(laz|las)$", recurse = TRUE)
  }))
  
  if (length(laz_files) == 0) stop("No LAS/LAZ files found in input_dirs.")
  
  ctg <- readLAScatalog(laz_files)
  if (!is.null(crs_target)) {
    projection(ctg) <- crs_target
  }
  
  # Configure tiling
  opt_chunk_size(ctg) <- tile_size
  opt_chunk_buffer(ctg) <- buffer
  opt_output_files(ctg) <- ""  # Don’t write by default, we control writing
  opt_progress(ctg) <- TRUE
  
  dir_create(processed_dir)
  
  catalog_apply(ctg, function(chunk, ...) {
    process_and_save_tile(chunk, processed_dir, buffer)
  })
  
  message("🎉 All tiles processed and saved in: ", processed_dir)
}



run_memory_efficient_workflow(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/LAZ31/",
    "F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/"
  ),
  processed_dir = "F:/MASTERS/THESIS/data/Processed/",
  tile_size = 1000,
  buffer = 30,
  crs_target = "EPSG:32149"
)













#
#
#
#
#
#
#
#














# Merging Counties OLD~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----

##function to merge laz files together and save as las

merge_laz_catalog <- function(input_dirs, output_file, crs_target = NULL, remove_duplicates = TRUE) {
  library(lidR)
  library(fs)
  library(dplyr)
  
  # 1. List all LAZ/LAS files
  laz_files <- unlist(lapply(input_dirs, function(dir) dir_ls(dir, regexp = "\\.(laz|las)$", recurse = TRUE)))
  
  if (length(laz_files) == 0) stop("No LAS/LAZ files found in input_dirs.")
  
  # 2. Create a LAScatalog from file paths
  ctg <- readLAScatalog(laz_files)
  
  # Optional: Force to a common CRS
  if (!is.null(crs_target)) {
    message("Setting target CRS: ", crs_target)
    projection(ctg) <- crs_target
  }
  
  # 3. Merge and optionally remove duplicates using catalog_apply
  process_chunk <- function(chunk, ...) {
    las <- readLAS(chunk)
    if (is.null(las) || npoints(las) == 0) return(NULL)
    
    if (remove_duplicates) {
      las@data <- dplyr::distinct(las@data)
    }
    
    return(las)
  }
  
  opt_independent_files(ctg) <- TRUE
  opt_chunk_size(ctg) <- 0
  
  # 4. Run the merge
  merged <- catalog_apply(ctg, process_chunk)
  
  # 5. Write to final output file
  if (!is.null(merged)) {
    writeLAS(merged, output_file)
    message("✅ Merged LAS file written to: ", output_file)
  } else {
    warning("⚠️ No data was processed. Check that input files are valid and non-empty.")
  }
}
merge_laz_catalog(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/LAZ31/",
    "F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/"
  ),
  output_file = "F:/MASTERS/THESIS/data/Merged/Patap_15_18.laz",
  crs_target = "EPSG:2283"  # Optional; set your known CRS if needed
)
## Turns out the files are too big to merge together. Going to try a retiling options which handles things in chunks


### Little Gunpowder Falls Merge ----

#Load laz files
#Harf2013 & Balt2015
merge_laz(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/LAZ34/",
    "F:/MASTERS/THESIS/data/Clip/LAZ30/"
  ),
  tags = c("Harf2013", "Balt2015"), #make sure tags are in correct order
  output_file = "F:/MASTERS/THESIS/data/Merged/LGunF2013.laz"
)

#Harf2020 & Balt2015
merge_laz(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/Harford_2020_BLK1/clipped_chunk_1.laz",
    "F:/MASTERS/THESIS/data/Clip/LAZ30/clipped_chunk_1.laz"
  ),
  tags = c("Harf2020", "Balt2015"),
  output_file = "F:/MASTERS/THESIS/data/Merged/LGunF2020.laz"
)

#Locations of laz data
# Harf2013 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ34/clipped_chunk_1.laz")
# Harf2020 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Harford_2020_BLK1/clipped_chunk_1.laz")
# Balt2015 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ30/clipped_chunk_1.laz")









### Patapsco Merge ----
#Check crs
ctg <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ31/clipped_chunk_2.laz")  # or a vector of files
projection(ctg)

ctg2 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/clipped_chunk_2.laz")  # or a vector of files
projection(ctg2)


#starting with tile size of 1000m. If running into memory issues, will try 500 or smaller. 
#Starting with buffer size of 30m. IF need more detail for trees, should use 50 or more. buffer adds extra space around each tile when it’s created. This helps mitigate edge effects, especially for algorithms that depend on surrounding points, like lasground() (ground classification), lastrees() (tree detection), or grid_metrics(). 

#Dealing with overlapping- currently this just removes exact duplicates. Should look into spatial thinning. I should consider in the areas where the counties overlap to look at point density???? I definitely need to address this our the raster products will look wonky. --need to clip buffer!!!

##Also need to add in info for each point so I know what year it is from and what county its from....

#Maybe to save on storage space we need to retile, process, and then save. instead of saving after retiling. 

##Retile lidar Function: 
#Read in Raw LAZ files. Set chunking sizes to 1000 meter x 1000m tile chunks with 30 meter buffers on all sides. Saves each tile into a 'Retiled' directory. 
retile_lidar <- function(input_dirs, retile_dir, tile_size, buffer, crs_target = NULL, tile_basename = "tile") {
  laz_files <- unlist(lapply(input_dirs, function(dir) {
    fs::dir_ls(dir, regexp = "\\.(laz|las)$", recurse = TRUE)
  }))
  
  if (length(laz_files) == 0) stop("No LAS/LAZ files found in input_dirs.")
  
  ctg <- lidR::readLAScatalog(laz_files)
  
  if (!is.null(crs_target)) {
    message("Setting target CRS: ", crs_target)
    projection(ctg) <- crs_target
  }
  
  fs::dir_create(retile_dir)
  
  lidR::opt_chunk_size(ctg) <- tile_size
  lidR::opt_chunk_buffer(ctg) <- buffer
  lidR::opt_output_files(ctg) <- file.path(retile_dir, paste0(tile_basename, "tile_{XLEFT}_{YBOTTOM}"))
  #lidR::opt_independent_files(ctg) <- TRUE #This line reads entire laz object and ignores all of the tiling we are trying to do. 
  
  invisible(catalog_apply(ctg, function(chunk, ...) {
    las <- readLAS(chunk)
    
    if (is.null(las) || npoints(las) == 0) {
      message("⚠️ Empty tile found, returning NULL")
      return(NULL)  # Must return NULL for empty tiles
    }
    
    message("📦 Retiling...")
    message("🟡 Tile has ", npoints(las), " points")
    
    return(las)  # triggers writing of this tile
  }))
  
  message("Retiling complete. Tiles saved to: ", retile_dir)
}

##Process_tile Function: 
#Loops over each tile in the retile directory. Removes exact duplicates, classifies ground points, and filters to ground, vegetation (classes 3-5) and last returns.Writes the filtered and classified tiles to the processed dir. This function currently only filters vegetation points if it is pre-classified in the original pointcloud. If it is not, it should only keep ground and surface points. 
process_tile <- function(lasfile, output_dir, remove_duplicates = TRUE) {
  las <- readLAS(lasfile)
  if (is.null(las) || npoints(las) == 0) {
    message("Skipping empty or invalid file: ", lasfile)
    return(NULL)
  }
  
  if (remove_duplicates) {
    las@data <- dplyr::distinct(las@data)
  }
  
  # Classify ground
  las <- lidR::classify_ground(las, algorithm = csf())
  
  # Filter to keep ground, vegetation, and last returns (surface)
  las <- lidR::lasfilter(las,
                         Classification %in% c(2:5) | ReturnNumber == NumberOfReturns)
  
  # Write as LAZ instead of LAS
  out_file <- file.path(output_dir, sub("\\.(las|laz)$", ".laz", basename(lasfile)))
  writeLAS(las, out_file)
  
  message("Processed tile saved: ", out_file)
  return(out_file)
}

run_workflow <- function(input_dirs, retile_dir, processed_dir, tile_size, buffer, crs_target = NULL, tile_basename = "tile") {
  # Retile first
  retile_lidar(input_dirs, retile_dir, tile_size, buffer, crs_target, tile_basename)
  
  # Process retiled tiles
  tile_files <- list.files(retile_dir, pattern = "\\.(las|laz)$", full.names = TRUE)
  dir_create(processed_dir)
  
  lapply(tile_files, function(f) process_tile(f, processed_dir))
  
  message("All tiles processed and saved in: ", processed_dir)
}

# Patap 2015/2018
run_workflow(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/LAZ31/",
    "F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/"
  ),
  retile_dir = "F:/MASTERS/THESIS/data/Retiled/",
  processed_dir = "F:/MASTERS/THESIS/data/Processed/",
  tile_size = 1000,
  buffer = 30,
  crs_target = "EPSG:32149",
  tile_basename = "Patap18_15"
)




# Patap 2011/2018
run_workflow(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/LAZ31/",
    "F:/MASTERS/THESIS/data/Clip/LAZ26/"
  ),
  retile_dir = "F:/MASTERS/THESIS/data/Retiled/",
  processed_dir = "F:/MASTERS/THESIS/data/Processed/",
  tile_size = 1000,
  buffer = 30,
  crs_target = "EPSG:32149",
  tile_basename = "Patap11_15"
)


##Once this is working, I should run_workflow on ALL files at once.


#Locations of laz files
# How2011a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_2.laz")
# How2011b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_3.laz")
# How2018 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/clipped_chunk_2.laz")
# Balt2015 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ31/clipped_chunk_2.laz")







### Patuxent

#Load laz files
How2011a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_2.laz")
How2011b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/LAZ26/clipped_chunk_3.laz")
How2018 <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/How_2018_BLK_1/clipped_chunk_3.laz")

### Seneca

#Load laz files
Mont2020a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Montgomery_2020_BLK2/clipped_chunk_4.laz")
Mont2020b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Montgomery_2020_BLK3/clipped_chunk_4.laz")
Mont2018a <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Mont_2018_BLK2/clipped_chunk_4.laz")
Mont2018b <- readLAScatalog("F:/MASTERS/THESIS/data/Clip/Mont_2018_BLK4/clipped_chunk_4.laz")

## Dealing with duplicates 

# Save as las file ----




















