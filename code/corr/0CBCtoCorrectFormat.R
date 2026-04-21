# Created 1/22/2026
# Last updated: 1/28/2026 - Anneliese Pinnell
# Updated 03/05/2026 w/testing and set year to be numeric

# The goal of this code is to convert the CBC data to the same 
# format as mBBS
# Needed columns: year, common_name, count, sci_name

library(tidyr)
library(dplyr)
library(testthat)
library(beepr)

makeSmallCSV <- function(fileName, skipNum, removeNum){
  #Skips weather data
  originalFile <- read.csv(fileName, skip = skipNum, header = TRUE)
  #Keeps only bird data (removes people)
  rowsToKeep <- head(originalFile, n = nrow(originalFile) - removeNum)
  
  #Splits COM_NAME into com_name and sci_name
  runningCSV <- rowsToKeep |> 
    separate(
      col = COM_NAME,
      into = c("common_name", "sci_name"),
      sep = "\n",
      convert = FALSE
    )
  # Removes brackets [] from sci_name
  runningCSV$sci_name <- gsub("\\[|\\]|\\{|\\}|\\(|\\)", "", runningCSV$sci_name)
  
  #Sources year from CountYear
  runningCSV$year <- as.numeric(substr(runningCSV$CountYear, start = 1, stop = 4))
  
  #Renames NumberByPartyHours to count
  runningCSV$NumberByPartyHours <- as.numeric(as.character(runningCSV$NumberByPartyHours))
  runningCSV$count <- runningCSV$NumberByPartyHours
  
  #Only includes wanted columns
  finalCSV <- runningCSV |> select(year, common_name, sci_name, count)
  finalCSV[finalCSV == ""] <- 0
  finalCSV[is.na(finalCSV)] <- 0
  
  return(finalCSV)
  
}

#NCCP
nccp <- makeSmallCSV("data/CBCHistoricData/HistoricalResultsByCount [NCCP-1901-2025].csv",
                     255, 1026)

#NCDU
ncdu <- makeSmallCSV("data/CBCHistoricData/HistoricalResultsByCount [NCDU-1901-2025].csv", 
                     195, 776)

#NCJL
ncjl <- makeSmallCSV("data/CBCHistoricData/HistoricalResultsByCount [NCJL-1901-2025].csv", 
                     159, 1237)
# Add filtering for CBC
# Only species found across all routes
allRouteBirds <- intersect(intersect(unique(nccp$common_name), unique(ncdu$common_name)), 
                           unique(ncjl$common_name))


#Need to combine csvs into one!
mergedDF <- rbind(nccp, ncdu)

#Must be seen on all routes
mergedDF <- rbind(mergedDF, ncjl) |>
  filter(common_name %in% allRouteBirds)

#Must be seen at least 13 years (26/2)
temp <- mergedDF |> 
  filter(count > 0) |>
  group_by(common_name) |>
  summarize(n_years = n_distinct(year)) |>
  ungroup() |>
  filter(n_years >= 13) |>
  select(common_name)

mergedDFFinal <- mergedDF |>
  filter(common_name %in% c(temp$common_name))

write.csv(mergedDFFinal, "data/CBCHistoricData/CBCMerged.csv", row.names = FALSE)

# TESTING - PASSED
testOutput <- makeSmallCSV("data/testingData/0CBCToCorrectTest.csv",
                           1, 0)

testExpected <- read.csv("data/testingData/0CBCToCorrectExpected.csv")

if(test_that("testing output matches expected output", 
          expect_equal(testOutput, testExpected))){
  beepr::beep(4)
}else{
  beepr::beep(9)
}
