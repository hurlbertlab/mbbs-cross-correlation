library(dplyr)
library(sf)
library(terra)
library(ebirdst)
library(tmap)

#set_ebirdst_access_key("ivvdjgaafpht")
#usethis::edit_r_environ()
cbc <- read.csv("data/traits/CBCTraits.csv")
mbbs <- read.csv("data/traits/mbbsTraits.csv")
names <- unique(c(unique(mbbs$English.Name..BirdLife...IOC...Clements.AviList.), 
                  unique(cbc$English.Name..BirdLife...IOC...Clements.AviList.)))

temp2 <- ebirdst_runs |>
  filter(common_name %in% names & species_code != "yebsap-example")

#Download ranges
for (spCode in temp2$species_code){
  ebirdst_download_status(species = spCode, download_ranges = TRUE, 
                          pattern = "raw_9km_")
}
