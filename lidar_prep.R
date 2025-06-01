##### ----- Lidar data preparation ----- #####
#### Author: Leah E. Staub
#### Creation Date: 04/25/2023
#### Update Date: 05/28/2025
#### Purpose: This script creates polygons based on study area raster extents, converts pre-downloaded laz files to las, and clips las files to study area extents. 

# Set up Environment ----
library(pacman)
p_load(mapview, sf, ggplot2, mapview, lidR, terra, tidyterra)

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






# Batch Clip Extents---- 
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

#Batch Clip Quantiles ----
#this is where we will make a mask of the quantile rasters and create polygons from them. 


#Lidar County Block Finder ----
# Plot the polygon interactively with a basemap and the MD iMAP lidar county blocks. Determine which blocks need to be downloaded for study area.
mapviewOptions(viewer.suppress = FALSE) #when set to TRUE: auto plots in firefox since R viewer is broken. Doesn't always work.

mapview(all_extents, col.regions = "red", alpha.regions = 0.5) +
  mapview(md_block, col.regions = "lightblue", alpha.regions = 0.5)


# Working with raw lidar files ----
#extract metadata for each block




















#Montgomery County Block 2
mont_lidar <- paste0("E:/MDLiDAR/Montgomery/Mont_2018_BLK2/LAZ")

mont_cat<- readLAScatalog(mont_lidar)
crs(mont_cat)
projection(mont_cat) <- "ESRI:103069"

# Convert catalog extent to sf polygons (footprints)
mont_cat_sf <- st_as_sf(mont_cat)

#View las catalog item with extent
mapview(bbox_sf, col.regions = "lightblue", alpha.regions = 0.5) + 
  mapview(mont_cat_sf, color = "red", layer.name = "LAS Catalog")

plot(mont_cat_proj, mapview=TRUE, map.type="Esri.WorldStreetMap")

crs(mont_cat_sf)

mont_cat_proj<- mont_cat%>% 
  st_set_crs("ESRI:103069")

st_crs(mont_cat_proj)

plot(mont_cat_proj, mapview=TRUE, map.type="Esri.WorldStreetMap")


#Montgomery County Block 3
mont_lidar <- paste0("E:/MDLiDAR/Montgomery/Montgomery_2020_BLK6/LAZ")

mont_cat<- readLAScatalog(mont_lidar)

plot(mont_cat, mapview=TRUE, map.type="Esri.WorldStreetMap")

st_crs(mont_cat)

mont_cat_proj<- mont_cat%>% 
  st_set_crs("ESRI:103069")

st_crs(mont_cat_proj)

plot(mont_cat_proj, mapview=TRUE, map.type="Esri.WorldStreetMap")

#Montgomery County Block 4
mont_lidar <- paste0("E:/MDLiDAR/Montgomery/Montgomery_2020_BLK6/LAZ")

mont_cat<- readLAScatalog(mont_lidar)

plot(mont_cat, mapview=TRUE, map.type="Esri.WorldStreetMap")

st_crs(mont_cat)

mont_cat_proj<- mont_cat%>% 
  st_set_crs("ESRI:103069")

st_crs(mont_cat_proj)

plot(mont_cat_proj, mapview=TRUE, map.type="Esri.WorldStreetMap")

#Montgomery County Block 6
mont_lidar <- paste0("E:/MDLiDAR/Montgomery/Mont_2018_BLK6/LAZ")

mont_cat<- readLAScatalog(mont_lidar)

plot(mont_cat, mapview=TRUE, map.type="Esri.WorldStreetMap")

st_crs(mont_cat)

mont_cat_proj<- mont_cat%>% 
  st_set_crs("ESRI:103069")

st_crs(mont_cat_proj)

plot(mont_cat_proj, mapview=TRUE, map.type="Esri.WorldStreetMap")

