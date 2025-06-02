##### ----- Lidar data preparation ----- #####
#### Author: Leah E. Staub
#### Creation Date: 04/25/2023
#### Update Date: 06/01/2025
#### Purpose: This script creates polygons based on study area raster extents, converts pre-downloaded laz files to las, and clips las files to study area extents. 

# Set up Environment ----
library(pacman)
p_load(mapview, sf, ggplot2, mapview, lidR, terra, tidyterra, fs)

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

## Batch Clip Quantiles ----
#this is where we will make a mask of the quantile rasters and create polygons from them. 


# Lidar County Block Finder ----
# Plot the polygon interactively with a basemap and the MD iMAP lidar county blocks. Determine which blocks need to be downloaded for study area.
mapviewOptions(viewer.suppress = FALSE) #when set to TRUE: auto plots in firefox since R viewer is broken. Doesn't always work.

mapview(all_extents, col.regions = "red", alpha.regions = 0.5) +
  mapview(md_block, col.regions = "lightblue", alpha.regions = 0.5)


# Working with raw lidar files ----
### Visualize Blocks downloaded to ensure we have the correct ones
#Unzip and read in laz files as lascatalog items
zip_files <- c("F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK2.zip", 
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK3.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK4.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Mont_2020_BLK2.zip", 
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Mont_2020_BLK3.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Mont_2020_BLK4.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_1.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_2.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_3.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Harford/2020/Harford_2020_BLK1.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Baltimore/2015/BLK_30.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Baltimore/2015/BLK_31.zip"
               )

#function for unzipping and saving footprints
qaqc_zip_lidar <- function(zip_path, crs_proj = "ESRI:103069") {
  # Create a unique temporary folder
  unzip_dir <- file.path(tempdir(), tools::file_path_sans_ext(basename(zip_path)))
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
  
  # Find the directory (or directories) containing laz files
  laz_dirs <- unique(dirname(laz_files))
  
  if (length(laz_dirs) > 1) {
    warning("Multiple .laz directories found. Using the top unzip directory for readLAScatalog().")
    las_folder <- unzip_dir
  } else {
    las_folder <- laz_dirs[1]
  }
  
  message("Reading LAScatalog from: ", las_folder)
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
#starting at 4:47 PM 
#
#
all_footprints <- list()

for (zip_file in zip_files) {
  footprints <- qaqc_zip_lidar(zip_file)
  zip_name <- tools::file_path_sans_ext(basename(zip_file))
  all_footprints[[zip_name]] <- footprints
}


# Add to your existing footprints list
blk32_footprints$sorc_zp <- "Baltimore_BLK_32"
mont4_footprints$sorc_zp <- "Mont_2018_BLK4"

#Keep only similar columns
common_cols <- intersect(names(all_footprints), names(mont4_footprints))
all_footprints <- all_footprints[, common_cols]
mont4_footprints <- mont4_footprints[, common_cols]

all_footprints <- rbind(all_footprints, mont4_footprints)

#Visualize footprints for all downloaded laz data to ensure correct files were downloaded.
#Load study areas shapefile
extents<- read_sf("F:/MASTERS/THESIS/data/extents/all_extents.shp")
# Combine the footprints list into one sf object
# Add a column to each footprint indicating its source
all_footprints_named <- lapply(names(all_footprints), function(name) {
  sf_obj <- all_footprints[[name]]
  sf_obj$source_zip <- name  # Add zip name as a new column
  return(sf_obj)
})
combined_footprints <- do.call(rbind, all_footprints_named)

#save
st_write(combined_footprints, "F:/MASTERS/THESIS/data/raw_lidar/all_lidar_footprints.shp")

# Plot with your study area polygons (`extents`)
mapview(extents, col.regions = "red", alpha.regions = 0.5) + 
  mapview(all_footprints, 
          color = "lightblue", 
          layer.name = "LAS Catalog",
          zcol = "sorc_zp",
          alpha.regions = 0.3
          )

mapview(blk31_footprints, col.regions = "red", alpha.regions = 0.5)



### Seneca First
#extract metadata for each block


# Clipping raw lidar to study area ----
#This workflow will unzip laz folders to a temp dir, -> extract footprint & save as shapefile -> clip las catalog item to study area -> save as las file -> clean up temp dir

#Load study areas shapefile
extents<- read_sf("F:/MASTERS/THESIS/data/extents/all_extents.shp")

#Set paths to each zipped laz folder
zip_files <- c("F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK2.zip", 
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK3.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2018/Mont_2018_BLK4.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Mont_2020_BLK2.zip", 
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Mont_2020_BLK3.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Montgomery/2020/Mont_2020_BLK4.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_1.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_2.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Howard/2018/How_2018_BLK_3.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Baltimore/2015/BLK_30.zip",
               "F:/MASTERS/THESIS/data/raw_lidar/Baltimore/2015/BLK_31.zip"
)

process_zip <- function(zip_path, study_area, output_dir, crs_proj = "ESRI:103069") {
  # Create a unique temporary folder
  unzip_dir <- file.path(tempdir(), tools::file_path_sans_ext(basename(zip_path)))
  dir_create(unzip_dir)
  
  message("Unzipping: ", zip_path)
  unzip(zip_path, exdir = unzip_dir)
  
  # Find .laz files
  laz_files <- dir(unzip_dir, recursive = TRUE, pattern = "\\.laz$", full.names = TRUE)
  if (length(laz_files) == 0) {
    warning("No .laz files found in: ", zip_path)
    dir_delete(unzip_dir)
    return(NULL)
  }
  
  # Read as LAScatalog
  cat_las <- readLAScatalog(dirname(laz_files[1]))
  projection(cat_las) <- crs_proj
  
  # Generate footprint and tag with source zip
  footprint <- st_as_sf(cat_las)
  footprint$source_zip <- tools::file_path_sans_ext(basename(zip_path))
  
  # Reproject study area
  study_area_proj <- st_transform(study_area, crs_proj)
  
  # Clip LAScatalog to study area
  clipped_catalog <- clip_roi(cat_las, study_area_proj)
  
  # Set output options
  zip_name <- tools::file_path_sans_ext(basename(zip_path))
  output_subdir <- file.path(output_dir, zip_name)
  dir_create(output_subdir)
  
  opt_output_files(clipped_catalog) <- file.path(output_subdir, "tile_{ID}")
  opt_laz_compression(clipped_catalog) <- TRUE
  
  message("Processing and writing clipped files to: ", output_subdir)
  catalog_apply(clipped_catalog, identity)  # Just triggers the write
  
  message("Cleaning up temporary files for: ", zip_name)
  dir_delete(unzip_dir)
  
  # Return both output path and footprint
  return(list(output_path = output_subdir, footprint = footprint))
}


#Run function 
all_outputs <- list()
all_footprints <- list()



for (zip_file in zip_files) {
  result <- process_zip(zip_file, study_area=extents, output_dir = "F:/MASTERS/THESIS/data/raw_lidar/Clip")
  if (!is.null(result)) {
    zip_name <- tools::file_path_sans_ext(basename(zip_file))
    all_outputs[[zip_name]] <- result$output_path
    all_footprints[[zip_name]] <- result$footprint
  }
}



#Save footprints as shapefile
combined_footprints <- do.call(rbind, all_footprints)

# Save as shapefile or geopackage
st_write(combined_footprints, "path/to/save/combined_footprints.shp", delete_dsn = TRUE)




#Harford did not like the crs assigned and plotted in West Virginia. lidR package does not have the ability to reproject lascatalog items until they have already been converted to las files. We will handle Harford county separately. 
zip_files <- c("F:/MASTERS/THESIS/data/raw_lidar/Harford/2020/Harford_2020_BLK1.zip", "F:/MASTERS/THESIS/data/raw_lidar/Harford/2013/Harford_2013_BLK34.zip")


harf1_cat <- readLAScatalog("F:/MASTERS/THESIS/data/raw_lidar/Harford/2020/Harford_2020_BLK1/Harford_2020_BLK1")
crs_info <- projection(harf1_cat)
print(crs_info)
#Not in same projection as other files, need to reproject
ctg_proj <- catalog_reproject(ctg, "ESRI:103069")
harf1_footprints <- st_as_sf(harf1_cat)

# Merging Counties ----

## Dealing with duplicates 

# Save as las file ----




















