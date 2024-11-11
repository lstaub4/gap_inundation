##### ----- SENECA CREEK CHM Creation----- #####
#### Author: Leah E. Staub
#### Creation Date: 04/25/2023
#### Update Date: 11/11/2024
#### Purpose: This script takes a DSM and DTM and creates a CHM from them. 

library(pacman)
p_load(lidR, terra)


#Load DSM
DSM<- rast("E:/MASTERS/THESIS/Scripts/DSMs/sen_dsm_pf3_clip.tif")
st_crs(DSM)
crs(DSM)<- "EPSG:2893"
DSM

#Load clipped DTM
DTM<- rast("E:/MASTERS/THESIS/TerrainFiles/SenecaTerrainmo/senterr_clip.vrt")
st_crs(DTM)
crs(DTM)<- "EPSG:2893"
DTM

plot(DSM)
plot(DTM)

#Do these rasters have different pixel sizes? 
#Let's resample to the smaller pixel size to be safe. 
#Hopefully they are on the same grid




