library(sf)
library(dplyr)
library(tmap)
library(spatialEco)
library(rnaturalearth)
library(geojsonio)

world <- ne_countries(returnclass = "sf")
americas <- world |>
  group_by(continent) |>
  filter(continent == "North America" 
         & geounit != "Greenland") |>
  st_crop(xmin = -179, ymin = -60, xmax = -20, ymax = 40)
#st_crop(xmin = -179, ymin = -60, xmax = -30, ymax = 45)
tm_shape(americas) + tm_borders(col = "green")

st_write(americas, "data/temperature/america.shp")
