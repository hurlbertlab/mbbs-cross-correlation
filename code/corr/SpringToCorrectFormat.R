# Created 1/22/2026
# Last updated: 1/22/2026 - Anneliese Pinnell
# 02/18/2026 - Updated counts to be divided by party hour

# The goal of this code is to convert the Spring data to the same 
# format as mBBS
# Needed columns: year, common_name, count, sci_name

library(tidyr)
library(dplyr)
library(readxl)

#Chapel Hill
fileName <- "data/Spring/CHSBC-all-years.xlsx"

rawData <- read_excel(fileName, skip=3)
rawData$sort <- NULL
rawData <- rawData %>% rename(`2013` = "2013...59", `2014` = "2014...60",
                              `2015` = "2015...61")
saveNames <- rawData$`COMMON NAME`

#Sets column names to be numbers
rawData <- rawData %>%
  mutate(across(where(is.character), as.numeric))
rawData$`COMMON NAME` <- saveNames

#Removes excess columns
rawData <- rawData[1:(length(rawData)-25)]

#Removes excess rows
cleanedData <- rawData[-c((nrow(rawData) - 49 + 1):nrow(rawData)), ]

#Stores excess rows and creates party hours data frame
removed_rows <- tail(rawData, n = 49)
party_hours <- removed_rows[!is.na(removed_rows$`COMMON NAME`) & removed_rows$`COMMON NAME` == "Party Hours", ]
cols_to_keep <- names(party_hours) >= 1999
party_hours <- party_hours[, cols_to_keep]

#Removes years before 1999 to cleanedData
cleanedData <- cleanedData[, cols_to_keep]

#Adds zeros to blank and NA values
cleanedData[is.na(cleanedData)] <- 0
cleanedData[cleanedData == ""] <- 0

#Divide values
partyHours <- as.numeric(party_hours[1, -1])
saveNames <- cleanedData$`COMMON NAME`
cleanedData <- cleanedData[, -1] / partyHours
cleanedData$"COMMON NAME" <- saveNames

# Pivot the data longer
finalCSV <- cleanedData %>%
  pivot_longer(cols = -"COMMON NAME", 
               names_to = "Year", 
               values_to = "Count") %>%
  mutate(Year = as.numeric(Year))

finalCSV <- rename(finalCSV, common_name = "COMMON NAME",  year = "Year", count = "Count")
write.csv(finalCSV, "data/Spring/NCCP.csv", row.names = FALSE)

#Jordan Lake
fileName <- "data/Spring/cumulative,jls,web.xlsx"

rawData <- read_excel(fileName, skip = 3)
rawData$`2020 order index` <- NULL
rawData$`abundance code*` <- NULL
rawData$`2020` <- NULL

#Removes excess columns
rawData <- rawData[1:(length(rawData)-3)]

#Removes excess rows
cleanedData <- rawData[-c((nrow(rawData) - 90):nrow(rawData)), ]

#Stores excess rows and creates party hours data frame
removed_rows <- tail(rawData, n = 90)
bothHours <- c("Diurnal party hours", "Nocturnal party hours")
hours <- removed_rows[!is.na(removed_rows$`COMMON NAME`) & removed_rows$`COMMON NAME` %in% bothHours, ]
cols_to_keep <- names(hours) >= 1999
hours <- hours[, cols_to_keep]
party_hours <- hours[1, -1] + hours[2, -1]
party_hours$`COMMON NAME` <- "Party hours"

#Removes years before 1999 to cleanedData
cleanedData <- cleanedData[, cols_to_keep]

#Adds zeros to blank and NA values
cleanedData[is.na(cleanedData)] <- 0
cleanedData[cleanedData == ""] <- 0

#Divide values
partyHours <- as.numeric(party_hours[,-ncol(party_hours)])
saveNames <- cleanedData$`COMMON NAME`
cleanedData <- cleanedData[, -1] / partyHours
cleanedData$"COMMON NAME" <- saveNames

# Pivot the data longer
JLFinal <- cleanedData %>%
  pivot_longer(cols = -"COMMON NAME", 
               names_to = "Year", 
               values_to = "Count") %>%
  mutate(Year = as.numeric(Year))

JLFinal <- rename(JLFinal, common_name = "COMMON NAME",  year = "Year", count = "Count")
write.csv(JLFinal, "data/Spring/NCJL.csv", row.names = FALSE)

#Combine CP and JL
df1 <- read.csv("data/Spring/NCCP.csv")
df2 <- read.csv("data/Spring/NCJL.csv")

#Need to combine csvs into one!
mergedDF <- rbind(df1, df2)

mergedDF[is.na(mergedDF)] <- 0
mergedDF[mergedDF == ""] <- 0

write.csv(mergedDF, "data/Spring/SpringMerged.csv", row.names = FALSE)
