# Created 02/17/2026
# Last updated 02/17/2026 by Anneliese Pinnell

# The purpose of this file is to pull bird data from AVONET and BIRDBASE
# and create a new dataframe

library(readxl)
library(tidyverse)

#Load in data
AVONET <- read_excel("data/AVONET.xlsx", sheet = "AVONET2_eBird")
BIRDBASE <- read_excel("data/BIRDBASE.xlsx", sheet = "Data")
mbbs <- read.csv("data/mbbs/mbbsLong.csv")

#Get list of names from mbbs data
uniqueNames <- unique(mbbs$common_name)

#Filter BIRDBASE and AVONET to only have needed rows
BBKeep <- c("English Name (BirdLife > IOC > Clements>AviList)", "AviList v1 2025",
            tail(names(BIRDBASE), n=81))

AVOKeep <- c("Species2", tail(names(AVONET), n=22))


BBFiltered <- BIRDBASE |>
  filter(`English Name (BirdLife > IOC > Clements>AviList)` %in% uniqueNames) |>
  select(all_of(BBKeep))

AVOFiltered <- AVONET |>
  filter(`Species2` %in% BBFiltered$"AviList v1 2025") |>
  select(all_of(AVOKeep))

#Join BIRDBASE and AVONET
combined <- left_join(BBFiltered, AVOFiltered, by = c("AviList v1 2025"="Species2"))

write_csv(combined, "data/mbbsTraits.csv")


