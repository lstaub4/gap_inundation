##### ----- Raw Point Cloud data Processing ----- #####
#### Author: Leah E. Staub
#### Creation Date: 07/06/2025
#### Update Date: 07/14/2025
#### Purpose: This script takes raw lidar point cloud data and classifies & filters it to ground, vegetation, and surface points. It does this by retiling the point cloud data for efficient processing.   

# Set up Environment ----
library(pacman)
p_load(mapview, sf, ggplot2, mapview, lidR, terra, tidyterra, fs, archive, tools, tidyverse)