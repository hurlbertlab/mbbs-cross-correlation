# Created 02/17/2026
# Last updated 02/17/2026 by Anneliese Pinnell
# 02/18/2026 - updated file so any file could get output instead of just mbbs

# The purpose of this file is to pull bird data from AVONET and BIRDBASE
# and create a new dataframe. Does not matter that the input file is not
# delta y because the names are the same for wide, long, and dY

library(readxl)
library(tidyverse)

#Load in data
AVONET <- read_excel("data/AVONET.xlsx", sheet = "AVONET2_eBird")
BIRDBASE <- read_excel("data/BIRDBASE.xlsx", sheet = "Data")

createCSV <- function(fileName, OutputCSVName){
  fileName <- read.csv(fileName)
  #Get list of names from data
  uniqueNames <- unique(fileName$common_name)
  
  #Filter BIRDBASE and AVONET to only have needed columns
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
  
  write_csv(combined, OutputCSVName)
}

#Create mbbs
createCSV("data/mbbs/mbbsLong.csv", "data/traits/mbbsTraits.csv")

#Create CBC
createCSV("data/CBCHistoricData/CBCMergedLong.csv", "data/traits/CBCTraits.csv")

#Create Resident
createCSV("data/residents/residentSpeciesLong.csv", "data/traits/residentTraits.csv")

