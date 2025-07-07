##### ----- Lidar data preparation ----- #####
#### Author: Leah E. Staub
#### Creation Date: 04/25/2023
#### Update Date: 07/06/2025
#### Purpose: This script creates polygons based on study area raster extents, converts pre-downloaded laz files to las, and clips las files to study area extents. 

# Set up Environment ----
library(pacman)
p_load(mapview, sf, ggplot2, mapview, lidR, terra, tidyterra, fs, archive, tools, tidyverse)

# Create clipping polygon ----
#Load MD county lidar blocks shapefile
md_block <- read_sf("F:/MASTERS/THESIS/data/pointclouds/Maryland_LiDAR_Status_-_LAZ_Download_Blocks.shp")
#Load terrain file used in HECRAS model
sen_ter<- rast("F:/MASTERS/THESIS/TerrainFiles/SenecaTerrainmo/SenecaTerrain/SenecaTerrain.tif")
#visualize
plot(sen_ter)
crs(sen_ter)
# Convert raster extent to polygon
#Get spatial extent of raster, note that crs is lost
bbox<- ext(sen_ter)
crs(bbox)
#convert the extent of raster to a polygon, making sure to add crs back
bbox_poly<- as.polygons(bbox, crs= crs(sen_ter))
#convert polygon to sf object
bbox_sf<-st_as_sf(bbox_poly)
#Save as shapefile
write_sf(bbox_sf, "F:/MASTERS/THESIS/data/extents/sen_extent.shp")




## Batch Clip Extents ---- 
#This section reads all of the tif tiles in a folder, converts and saves their bounding boxes as polygons.

dir <- "F:/MASTERS/THESIS/data/terraintifs"
tif_ls <- list.files(path = dir, pattern = "\\.tif$", full.names = TRUE)
n_tif<- length(tif_ls)

bbox_list <- list()
#Since Lambert Conformal Conic is an old WKT that r packages do not understand, we need to give everything a crs. 
# Use the CRS of the first file as the target CRS
first_rast <- rast(tif_ls[1])
first_bbox <- as.polygons(ext(first_rast), crs = crs(first_rast))
target_crs <- st_crs(st_as_sf(first_bbox))

for (f in seq_along(tif_ls)) {
  filename <- tif_ls[f]
  
  # Load raster and get extent
  extent <- rast(filename)
  bbox <- ext(extent)
  bbox_poly <- as.polygons(bbox, crs = crs(extent))
  bbox_sf <- st_as_sf(bbox_poly)
  
  # Check CRS; if different or missing, force it
  if (is.na(st_crs(bbox_sf)) || !st_crs(bbox_sf) == target_crs) {
    bbox_sf <- st_set_crs(bbox_sf, target_crs)
  }
  
  # Add filename as an attribute
  bbox_sf$file <- basename(filename)
  
  # Store in list
  bbox_list[[f]] <- bbox_sf
  
  # Save individual shapefile
  out_name <- paste0(
    "F:/MASTERS/THESIS/data/extents/",
    tools::file_path_sans_ext(basename(filename)),
    "_extent.shp"
  )
  write_sf(bbox_sf, out_name)
  
  print(paste("Saved:", out_name))
}

# Combine all bounding boxes into one sf object
all_extents <- do.call(rbind, bbox_list)
#save out as 1 file
write_sf(all_extents, "F:/MASTERS/THESIS/data/extents/all_extents.shp")

## Batch Clip Quantiles Extents ----
#this is where we will make a mask of the quantile rasters and create polygons from them. 


# Lidar County Block Finder ----
# Plot the polygon interactively with a basemap and the MD iMAP lidar county blocks. Determine which blocks need to be downloaded for study area.
mapviewOptions(viewer.suppress = FALSE) #when set to TRUE: auto plots in firefox since R viewer is broken. Doesn't always work.

mapview(all_extents, col.regions = "red", alpha.regions = 0.5) +
  mapview(md_block, col.regions = "lightblue", alpha.regions = 0.5)


# Laz file paths ----
#Paths to laz files for folder structure type 1 (still zipped)
zip_files <- c("F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK2.zip", 
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK4.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_1.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_2.zip"
)

#Paths to laz files for folder structure type 2 (manually unzipped)
unzip_files <- c("F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Montgomery_2020_BLK2/Montgomery_2020_BLK2", 
                 "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Montgomery_2020_BLK3/Montgomery_2020_BLK3",
                 "F:/MASTERS/THESIS/data/raw_lidar/Baltimore/2015/BLK_30/BLK_30/LAZ30",
                 "F:/MASTERS/THESIS/data/raw_lidar/Baltimore/2015/BLK_31/BLK_31/LAZ31",
                 "F:/MASTERS/THESIS/data/raw_lidar/Harford/2013/Harford_2013_BLK34/BLK_34/LAZ34",
                 "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2013/BLK_12/BLK_12/LAZ12",
                 "F:/MASTERS/THESIS/data/raw_lidar/Howard/2011/BLK_26/BLK_26/LAZ26"
)

#Paths to laz files with different crs than the rest 
harf_files <- c("F:/MASTERS/THESIS/data/raw_lidar/Harford/2020/Harford_2020_BLK1.zip") 

# Working with raw lidar files ----
### Visualize Blocks downloaded to ensure we have the correct ones
#Unzip and read in laz files as lascatalog items

#function for unzipping and saving footprints for folder structure type 1
qaqc_zip_lidar <- function(zip_path, crs_proj = "ESRI:103069", temp_root = "F:/MASTERS/THESIS/data/raw_lidar/tmp_unzip") {
  # Create a unique temp folder in user-specified writable location
  zip_name <- file_path_sans_ext(basename(zip_path))
  unzip_dir <- file.path(temp_root, zip_name)
  dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Unzipping: ", zip_path)
  unzip(zip_path, exdir = unzip_dir)
  
  # Recursively find all .laz files
  all_files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)
  laz_files <- all_files[grepl("\\.laz$", all_files, ignore.case = TRUE)]
  
  # Check if any laz files were found
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", zip_path)
    unlink(unzip_dir, recursive = TRUE)
    return(NULL)
  }
  
  # Dynamically get the top-level folder containing laz files
  las_folder <- dirname(laz_files[1])  # Use directory of first laz file
  message("Reading LAScatalog from unzip folder: ", las_folder)
  
  cat_las <- readLAScatalog(las_folder)
  
  message("Projection:", print(projection(cat_las)))
  projection(cat_las) <- crs_proj
  
  footprints_sf <- st_as_sf(cat_las)
  
  message("Cleaning up temporary files...")
  unlink(unzip_dir, recursive = TRUE)
  
  message("Done processing: ", zip_path)
  return(footprints_sf)
}

#function for grabbing the footprints of manually unzipped areas
qaqc_lidar <- function(unzip_path, crs_proj = "ESRI:103069") {
  # Recursively find all .laz files in the provided folder
  all_files <- list.files(unzip_path, recursive = TRUE, full.names = TRUE)
  laz_files <- all_files[grepl("\\.laz$", all_files, ignore.case = TRUE)]
  
  # Check if any laz files were found
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", unzip_path)
    return(NULL)
  }
  
  # Get the folder where the laz files are located
  las_folder <- dirname(laz_files[1])
  message("Reading LAScatalog from folder: ", las_folder)
  
  cat_las <- readLAScatalog(las_folder)
  
  message("Original projection:", print(projection(cat_las)))
  projection(cat_las) <- crs_proj
  
  footprints_sf <- st_as_sf(cat_las)
  
  message("Done processing: ", unzip_path)
  return(footprints_sf)
}

#Function for grabbing footprints of zipped laz files with different crs than the rest
qaqc_zip_lidar_crs <- function(zip_path, crs_proj = "EPSG:6487", temp_root = "F:/MASTERS/THESIS/data/raw_lidar/tmp_unzip") {
  # Create a unique temp folder in user-specified writable location
  zip_name <- file_path_sans_ext(basename(zip_path))
  unzip_dir <- file.path(temp_root, zip_name)
  dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Unzipping: ", zip_path)
  unzip(zip_path, exdir = unzip_dir)
  
  # Recursively find all .laz files
  all_files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)
  laz_files <- all_files[grepl("\\.laz$", all_files, ignore.case = TRUE)]
  
  # Check if any laz files were found
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", zip_path)
    unlink(unzip_dir, recursive = TRUE)
    return(NULL)
  }
  
  # Dynamically get the top-level folder containing laz files
  las_folder <- dirname(laz_files[1])  # Use directory of first laz file
  message("Reading LAScatalog from unzip folder: ", las_folder)
  
  cat_las <- readLAScatalog(las_folder)
  
  message("Projection:", print(projection(cat_las)))
  projection(cat_las) <- crs_proj
  
  footprints_sf <- st_as_sf(cat_las)
  
  message("Cleaning up temporary files...")
  unlink(unzip_dir, recursive = TRUE)
  
  message("Done processing: ", zip_path)
  return(footprints_sf)
}

# Begin the unzipping and footprint generation process
#starting at 3:58 PM 

all_footprints <- list()

for (zip_file in zip_files) {
  footprints <- qaqc_zip_lidar(zip_file)
  zip_name <- tools::file_path_sans_ext(basename(zip_file))
  all_footprints[[zip_name]] <- footprints
}

for (zip_file in unzip_files) {
  footprints <- qaqc_lidar(zip_file)
  zip_name <- tools::file_path_sans_ext(basename(zip_file))
  all_footprints[[zip_name]] <- footprints
}

for (zip_file in harf_files) {
  footprints <- qaqc_zip_lidar_crs(zip_file)
  zip_name <- tools::file_path_sans_ext(basename(zip_file))
  all_footprints[[zip_name]] <- footprints
}


#Visualize footprints for all downloaded laz data to ensure correct files were downloaded.
#Load study areas shapefile
extents<- read_sf("F:/MASTERS/THESIS/data/extents/all_extents.shp")
# Combine the footprints list into one sf object and add a column that indicates which zip folder each group of laz files came from.
#Harford county is a different crs, lets make sure everything is the same crs
target_crs <- st_crs(extents)

all_footprints_named <- lapply(names(all_footprints), function(name) {
  sf_obj <- all_footprints[[name]]
  
  # Reproject if CRS doesn't match target
  if (!is.null(sf_obj) && st_crs(sf_obj) != target_crs) {
    sf_obj <- st_transform(sf_obj, target_crs)
  }
  
  sf_obj$source_zip <- name  # Add zip name as a new column
  return(sf_obj)
})

combined_footprints <- do.call(rbind, all_footprints_named)

#save
st_write(combined_footprints, "F:/MASTERS/THESIS/data/raw_lidar/all_lidar_footprints.shp", append=FALSE)

#read in footprints


# Plot with your study area polygons (`extents`)
mapview(extents, col.regions = "red", alpha.regions = 0.5) + 
  mapview(combined_footprints, 
          color = "lightblue", 
          layer.name = "LAS Catalog",
          zcol = "source_zip",
          alpha.regions = 0.3
          )

mapview(blk31_footprints, col.regions = "red", alpha.regions = 0.5)

# Clipping raw lidar to study area ----
#This workflow will unzip laz folders to a temp dir, -> clip las catalog item to study area -> save as las file -> clean up temp dir

#Load study areas shapefile
extents<- read_sf("F:/MASTERS/THESIS/data/extents/all_extents.shp")

#Function for zipped files
clip_zip_lidar <- function(zip_path, 
                           clip_polygons, 
                           crs_proj = "ESRI:103069", 
                           temp_root = "F:/MASTERS/THESIS/data/raw_lidar/tmp_unzip", 
                           output_dir = "F:/MASTERS/THESIS/data/Clip") {
  
  # Create a unique temp folder
  zip_name <- file_path_sans_ext(basename(zip_path))
  unzip_dir <- file.path(temp_root, zip_name)
  dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Unzipping: ", zip_path)
  unzip(zip_path, exdir = unzip_dir)
  
  # Find all laz files
  laz_files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.laz$")
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", zip_path)
    unlink(unzip_dir, recursive = TRUE)
    return(NULL)
  }
  
  las_folder <- dirname(laz_files[1])
  cat_las <- readLAScatalog(las_folder)
  
  # Set projection if needed
  projection(cat_las) <- crs_proj
  
  # Reproject clip polygons to match LAS CRS
  clip_polygons_proj <- st_transform(extents, crs = crs_proj)
  
  # Set output path for clipped files
  clip_output_dir <- file.path(output_dir, zip_name)
  dir.create(clip_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Set catalog options
  opt_output_files(cat_las) <- file.path(clip_output_dir, "clipped_chunk_{ID}")
  opt_chunk_buffer(cat_las) <- 30
  opt_chunk_size(cat_las) <- 0  # disable spatial chunking
  opt_laz_compression(cat_las) <- TRUE
  
  message("Clipping point clouds...")
  
  # Perform the clipping
  clipped_catalog <- clip_roi(cat_las, clip_polygons_proj, inside = TRUE)
  
  message("Cleaning up temp files...")
  unlink(unzip_dir, recursive = TRUE)
  
  message("Done: ", zip_path)
  return(clipped_catalog)
}

#function for unzipped files
clip_unzipped_lidar <- function(unzip_path,
                                clip_polygons,
                                crs_proj = "ESRI:103069",
                                output_dir = "F:/MASTERS/THESIS/data/Clip") {
  # Find all laz files
  laz_files <- list.files(unzip_path, recursive = TRUE, full.names = TRUE, pattern = "\\.laz$", ignore.case = TRUE)
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", unzip_path)
    return(NULL)
  }
  
  # Set up LAScatalog
  las_folder <- dirname(laz_files[1])
  message("Reading LAScatalog from: ", las_folder)
    zip_name <- file_path_sans_ext(basename(unzip_path))
  cat_las <- readLAScatalog(las_folder)
  projection(cat_las) <- crs_proj
  
  # Set projection if needed
  projection(cat_las) <- crs_proj
  
  # Reproject clip polygons to match LAS CRS
  clip_polygons_proj <- st_transform(extents, crs = crs_proj)
  
  # Set output path for clipped files
  clip_output_dir <- file.path(output_dir, zip_name)
  dir.create(clip_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Set catalog options
  opt_output_files(cat_las) <- file.path(clip_output_dir, "clipped_chunk_{ID}")
  opt_chunk_buffer(cat_las) <- 30
  opt_chunk_size(cat_las) <- 0  # disable spatial chunking
  opt_laz_compression(cat_las) <- TRUE
  
  message("Clipping point clouds...")
  
  # Perform the clipping
  clipped_catalog <- clip_roi(cat_las, clip_polygons_proj, inside = TRUE)
  
  message("Done: ", unzip_path)
  return(clipped_catalog)
}

#function for different crs
clip_zip_lidar_crs <- function(zip_path,
                               clip_polygons,
                               crs_proj = "EPSG:6487",
                               temp_root = "F:/MASTERS/THESIS/data/raw_lidar/tmp_unzip",
                               output_dir = "F:/MASTERS/THESIS/data/Clip") {
  # Create a unique temp folder
  zip_name <- file_path_sans_ext(basename(zip_path))
  unzip_dir <- file.path(temp_root, zip_name)
  dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("Unzipping: ", zip_path)
  unzip(zip_path, exdir = unzip_dir)
  
  # Find all laz files
  laz_files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.laz$")
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", zip_path)
    unlink(unzip_dir, recursive = TRUE)
    return(NULL)
  }
  
  las_folder <- dirname(laz_files[1])
  cat_las <- readLAScatalog(las_folder)
  
  # Set projection if needed
  projection(cat_las) <- crs_proj
  
  # Reproject clip polygons to match LAS CRS
  clip_polygons_proj <- st_transform(extents, crs = crs_proj)
  
  # Set output path for clipped files
  clip_output_dir <- file.path(output_dir, zip_name)
  dir.create(clip_output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Set catalog options
  opt_output_files(cat_las) <- file.path(clip_output_dir, "clipped_chunk_{ID}")
  opt_chunk_buffer(cat_las) <- 30
  opt_chunk_size(cat_las) <- 0  # disable spatial chunking
  opt_laz_compression(cat_las) <- TRUE
  
  message("Clipping point clouds...")
  
  # Perform the clipping
  clipped_catalog <- clip_roi(cat_las, clip_polygons_proj, inside = TRUE)
  
  message("Cleaning up temp files...")
  unlink(unzip_dir, recursive = TRUE)
  
  message("Done: ", zip_path)
  return(clipped_catalog)
}

#Apply functions
all_clipped <- list()

# Zip files
for (zip_file in zip_files) {
  message("Processing file: ", zip_file)
  
  clipped_result <- clip_zip_lidar(
    zip_path = zip_file,
    clip_polygons = clip_shapes,
    crs_proj = "ESRI:103069",  # or match whatever CRS you’re using
    temp_root = "F:/MASTERS/THESIS/data/raw_lidar/tmp_unzip",
    output_dir = "F:/MASTERS/THESIS/data/Clip"
  )
  
  # Save result object if needed
  zip_name <- tools::file_path_sans_ext(basename(zip_file))
  all_clipped[[zip_name]] <- clipped_result
}

#unzipped files
for (unzipped_path in unzip_files) {
  message("\n--- Processing: ", unzipped_path, " ---")
  clip_unzipped_lidar(
    unzip_path = unzipped_path,
    clip_polygons = clip_shapes,
    crs_proj = "ESRI:103069",
    output_dir = "F:/MASTERS/THESIS/data/Clip"
  )
}

#crs file
for (zip_file in harf_files) {
  message("Processing file: ", zip_file)
  
  clipped_result <- clip_zip_lidar_crs(
    zip_path = zip_file,
    clip_polygons = clip_shapes,
    crs_proj = "EPSG:6487",  # or match whatever CRS you’re using
    temp_root = "F:/MASTERS/THESIS/data/raw_lidar/tmp_unzip",
    output_dir = "F:/MASTERS/THESIS/data/Clip"
  )
}

# Merging Counties ----

# Function to merge LAZ files for a study area
merge_laz <- function(input_dirs, tags, output_file, remove_duplicates = TRUE) {
  if (length(input_dirs) != length(tags)) {
    stop("The number of input directories must match the number of tags.")
  }
  
  all_las <- list()
  
  for (i in seq_along(input_dirs)) {
    laz_files <- dir_ls(input_dirs[i], regexp = "\\.(laz|las)$", recurse = TRUE)
    for (file in laz_files) {
      las <- readLAS(file)
      if (!is.null(las)) {
        las@data$source_id <- tags[i]  # Add source tag
        all_las[[length(all_las) + 1]] <- las
      }
    }
  }
  
  # Merge all tagged LAS objects
  las_merged <- do.call(rbind, all_las)
  
  # Remove exact duplicates if requested
  if (remove_duplicates && !is.null(las_merged)) {
    las_merged@data <- distinct(las_merged@data)  # dplyr::distinct handles data.frame-level uniqueness
  }
  
  # Save merged LAS/LAZ
  if (!is.null(las_merged)) {
    writeLAS(las_merged, output_file)
    message("Merged LAS file written to: ", output_file)
  } else {
    warning("Merge resulted in a NULL object.")
  }
}



##New function for merge that is better for memory....
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

#New.....
merge_laz_catalog(
  input_dirs = c(
    "F:/MASTERS/THESIS/data/Clip/LAZ31/",
    "F:/MASTERS/THESIS/data/Clip/How_2018_BLK_2/"
  ),
  output_file = "F:/MASTERS/THESIS/data/Merged/Patap_15_18.laz",
  crs_target = "EPSG:2283"  # Optional; set your known CRS if needed
)

###Visualize overlaps
# List all LAZ files
laz_files <- list.files(input_dirs, pattern = "\\.(laz|las)$", full.names = TRUE, recursive = TRUE)

# Create a data frame of bounding boxes
las_bounds <- lapply(laz_files, function(file) {
  header <- readLASheader(file)
  bbox <- header@PHB
  data.frame(
    filename = basename(file),
    xmin = bbox$`Min X`, xmax = bbox$`Max X`,
    ymin = bbox$`Min Y`, ymax = bbox$`Max Y`
  )
}) %>%
  bind_rows()

#convert to poly
las_polys <- las_bounds %>%
  rowwise() %>%
  mutate(geometry = list(st_polygon(list(matrix(
    c(xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin), 
    ncol = 2, byrow = TRUE))))) %>%
  st_as_sf(crs = "+proj=lcc +lat_0=37.6666666666667 +lon_0=-77 +lat_1=39.45 +lat_2=38.3 +x_0=399999.9998984 +y_0=0 +ellps=GRS80 +units=us-ft +no_defs")

#Visualize
overlaps <- st_intersection(las_polys)
ggplot(overlaps) +
  geom_sf(fill = "red", alpha = 0.4) +
  ggtitle("Overlapping Areas Between LAS Files")

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




















