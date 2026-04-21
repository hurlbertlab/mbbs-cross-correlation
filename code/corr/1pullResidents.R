# Created 02/24/2026 

# The goal of this file is to get the resident species amounts and create a 
# file for them
library(pheatmap)
library(correlation)
library(tidyverse)

mbbs <- read.csv("data/mbbs/mbbsMerged.csv")
cbc <- read.csv("data/CBCHistoricData/CBCMerged.csv")

mbbsSp <- unique(mbbs$common_name)
cbcSp <- unique(cbc$common_name)
inBoth <- intersect(mbbsSp, cbcSp)

cbc <- cbc |>
  filter(common_name %in% inBoth)

mbbs <- mbbs |>
  filter(common_name %in% inBoth)

#To csv file
write.csv(cbc, "data/residents/CBCRawLong.csv", row.names = FALSE)
write.csv(mbbs, "data/residents/mbbsRawLong.csv", row.names = FALSE)