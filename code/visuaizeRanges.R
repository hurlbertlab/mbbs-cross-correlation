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
name <- "carchi"
formatFileName <- c("data/ranges/2023/", name, "/ranges/",
                    name, "_range_raw_9km_2023.gpkg")
attemptLoad <- st_read(paste(formatFileName, collapse = ""))|>
  filter(season == "nonbreeding" | season == "resident")
# Can't use DAYMET as server doesn't exist anymore whoops

tmin2020Dec <- rast("data/temperature/tmin2020-2024/wc2.1_cruts4.09_2.5m_tmin_2020-12.tif")

extractedTemperatures <- extract(tmin2020Dec, attemptLoad, xy = TRUE)
extractedTemperatures <- st_as_sf(extractedTemperatures, coords = c("x", "y"), crs = 4326)
minimumTempMean5Perc <- extractedTemperatures |>
  arrange(wc2.1_2.5m_tmin_12) |>
  head(as.integer(length(extractedTemperatures$wc2.1_2.5m_tmin_12)*0.05)) |>
  summarise(meanTemp = mean(wc2.1_2.5m_tmin_12))

p1 <- tm_shape(extractedTemperatures) + 
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_dots(fill = "wc2.1_2.5m_tmin_12", fill.legend = tm_legend(title = "Temperature (Celcius)",
                                                               reverse = TRUE),
        fill.scale = tm_scale(values = "-brewer.rd_yl_bu", breaks = c(-Inf ,-5, -2.5, 0, 2.5, 5, Inf))) +
  tm_shape(attemptLoad) +
  tm_borders(lwd = 1) +
  tm_title("Carolina Chickadee Range - Minimum Temperature", 
           size = 1,
           fontface = "bold"
  ) + tm_title("December 2020", size = 0.75, fontface = "bold")

tmin2024Dec <- rast("data/temperature/tmin2020-2024/wc2.1_cruts4.09_2.5m_tmin_2024-12.tif")

extractedTemperatures <- extract(tmin2024Dec, attemptLoad, xy = TRUE)
extractedTemperatures <- st_as_sf(extractedTemperatures, coords = c("x", "y"), crs = 4326)
minimumTempMean5Perc <- extractedTemperatures |>
  arrange(wc2.1_2.5m_tmin_12) |>
  head(as.integer(length(extractedTemperatures$wc2.1_2.5m_tmin_12)*0.05)) |>
  summarise(meanTemp = mean(wc2.1_2.5m_tmin_12))

p2 <- tm_shape(extractedTemperatures) + 
  tm_basemap("Esri.WorldGrayCanvas") +
  tm_dots(fill = "wc2.1_2.5m_tmin_12", fill.legend = tm_legend(title = "Temperature (Celcius)",
                                                               reverse = TRUE),
          fill.scale = tm_scale(values = "-brewer.rd_yl_bu", breaks = c(-Inf ,-5, -2.5, 0, 2.5, 5, Inf))) +
  tm_shape(attemptLoad) +
  tm_borders(lwd = 1) +
  tm_title("Carolina Chickadee Range - Minimum Temperature", 
           size = 1,
           fontface = "bold"
  ) + tm_title("December 2020", size = 0.75, fontface = "bold")

png(filename = "figures/temperature/chickadee1.png", width = 600, height = 500)
p1
dev.off()
p2

# Trying to get better temperature data
library(appeears)
library(keyring)
options(keyring_backend = "file")
rs_set_key(user = "apinnell")
token <- rs_login(user = "apinnell")

temp2 <- st_as_sf(attemptLoad)
class(temp2)

taskDf <- data.frame(
  task = "Daymet_Study",
  subtask = "Site_1",
  latitude = 42.5378,
  longitude = -72.1715,
  start = "2024-01-01",
  end = "2024-12-31",
  product = "DAYMET",
  layer = c("tmin")
)

# Build and request
task <- rs_build_task(df = taskDf, roi = temp2, format = "geotiff")
response <- rs_request(request = task, transfer = TRUE)
Sys.setenv(EARTHDATA_USER = "apinnell")
Sys.setenv(EARTHDATA_PASS = "EDL12370860p!$")

