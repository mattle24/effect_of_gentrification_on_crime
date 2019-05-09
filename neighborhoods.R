library(geojsonsf)
library(sf)

phily_neighborhoods <- read_sf("C:/Users/Matt_2/Downloads/Neighborhoods_Philadelphia")
# https://pittsburghpa.maps.arcgis.com/apps/OnePane/basicviewer/index.html?appid=7b284a2998454505a6f000d24ee1ded5
pitt_neighborhoods <- geojson_sf("C:/Users/Matt_2/Downloads/Neighborhoods_with_SNAP_Data.geojson")
la_neighborhoods <- geojson_sf("C:/Users/Matt_2/Downloads/Neighborhoods.geojson")
sf_neighborhoods <- geojson_sf("C:/Users/Matt_2/Downloads/Analysis Neighborhoods.geojson")
