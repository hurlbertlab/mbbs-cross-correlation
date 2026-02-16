# Created 1/22/2026
# Last updated: 1/22/2026 - Anneliese Pinnell

# The goal of this code is to convert the Spring data to the same 
# format as mBBS
# Needed columns: year, common_name, count, sci_name

library(tidyr)
library(dplyr)
library(readxl)

#Chapel Hill
fileName <- "data/Spring/CHSBC-all-years.csv"

originalFile <- read.csv(fileName, check.names = FALSE)
originalFile$sort <- NULL
#Adds zeros to blank and NA values
originalFile[is.na(originalFile)] <- 0
originalFile[originalFile == ""] <- 0

# Pivot the data longer
finalCSV <- originalFile %>%
  pivot_longer(cols = -"COMMON NAME", 
               names_to = "Year", 
               values_to = "Count") %>%
  mutate(Year = as.numeric(Year))

finalCSV <- rename(finalCSV, common_name = "COMMON NAME",  year = "Year", count = "Count")
write.csv(finalCSV, "data/Spring/NCCP.csv", row.names = FALSE)

#Jordan Lake
JordanLake <- read.csv("data/Spring/cumulativejlsweb-1.csv", check.names = FALSE)
JordanLake[is.na(JordanLake)] <- 0
JordanLake[JordanLake == ""] <- 0

# Pivot the data longer
JLFinal <- JordanLake %>%
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
