# Created 1/22/2026
# Last updated: 1/22/2026 - Anneliese Pinnell

# The goal of this code is to convert the CBC data to the same 
# format as mBBS
# Needed columns: year, common_name, count, sci_name
# Year is in CountYear first 4 char
# common_name is before [] in COM_NAME
# count is how_manyCW
# sci name is within [] of COM_NAME

library(tidyr)
library(dplyr)


makeSmallCSV <- function(fileName, outName, skipNum, removeNum){
  #Skips weather data
  originalFile <- read.csv(fileName, skip = skipNum, header = TRUE)
  #Keeps only bird data (removes people)
  rowsToKeep <- head(originalFile, n = nrow(originalFile) - removeNum)
  
  #Splits COM_NAME into com_name and sci_name
  runningCSV <- rowsToKeep %>% 
    separate(
      col = COM_NAME,
      into = c("common_name", "sci_name"),
      sep = "\n",
      convert = FALSE
    )
  # Removes brackets [] from sci_name
  runningCSV$sci_name <- gsub("\\[|\\]|\\{|\\}|\\(|\\)", "", runningCSV$sci_name)
  
  #Sources year from CountYear
  runningCSV$year <- substr(runningCSV$CountYear, start = 1, stop = 4)
  
  #Renames how_manyCW to count
  runningCSV$count <- runningCSV$how_manyCW
  
  #Only includes wanted columns
  finalCSV <- runningCSV %>% select(year, common_name, sci_name, count)
  finalCSV$count <- as.integer(finalCSV$count)
  
  write.csv(finalCSV, outName, row.names = FALSE)
  
}

#NCCP
makeSmallCSV("data/CBCHistoricData/HistoricalResultsByCount [NCCP-1901-2025].csv",
             "data/CBCHistoricData/CBCRaw/CBCRawCP.csv", 255, 1026)

#NCDU
makeSmallCSV("data/CBCHistoricData/HistoricalResultsByCount [NCDU-1901-2025].csv",
             "data/CBCHistoricData/CBCRaw/CBCRawDU.csv", 195, 776)

#NCJL
makeSmallCSV("data/CBCHistoricData/HistoricalResultsByCount [NCJL-1901-2025].csv",
             "data/CBCHistoricData/CBCRaw/CBCRawJL.csv", 159, 1237)

# Example dataframes
df1 <- read.csv("data/CBCHistoricData/CBCRaw/CBCRawCP.csv")
df2 <- read.csv("data/CBCHistoricData/CBCRaw/CBCRawDU.csv")
df3 <- read.csv("data/CBCHistoricData/CBCRaw/CBCRawJL.csv")


#Need to combine csvs into one!
mergedDF <- rbind(df1, df2)
mergedDFFinal <- rbind(mergedDF, df3)

mergedDFFinal[mergedDFFinal == ""] <- 0
mergedDFFinal[is.na(mergedDFFinal)] <- 0

write.csv(mergedDFFinal, "data/CBCHistoricData/CBCMerged.csv", row.names = FALSE)

