library(dplyr)
library(sf)
library(terra)
library(tmap)
library(geodata)
library(ggplot2)
library(patchwork)

codeNames <- list.files("data/ranges/2023", full.names = FALSE)
tmap_mode("plot")
suppressWarnings(
suppressMessages(
for (name in codeNames){
  formatFileName <- c("data/ranges/2023/", name, "/ranges/",
                      name, "_range_raw_9km_2023.gpkg")
  attemptLoad <- st_read(paste(formatFileName, collapse = ""))|>
    filter(season == "nonbreeding" | season == "resident")
  png(filename = paste(c("figures/ranges/",name, ".png"), collapse = ""), width = 600, height = 600)
  print(tm_shape(attemptLoad) +
    tm_basemap("Esri.WorldStreetMap") +
    tm_polygons(col = "season",  fill = "blue",
                fill.legend = tm_legend(col.title = name),
                fill_alpha = 0.4) +
    tm_legend(legend_position = c("right", "bottom")) +
    tm_title(paste(name, "Range by Season (2023)")))
  dev.off()
}
))


# Example - Carolina Chickadee
name <- "comgra"
formatFileName <- c("data/ranges/2023/", name, "/ranges/",
                    name, "_range_raw_9km_2023.gpkg")
attemptLoad <- st_read(paste(formatFileName, collapse = ""))|>
  filter(season == "nonbreeding" | season == "resident")
# Can't use DAYMET as server doesn't exist anymore whoops

