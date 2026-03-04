# File created 03/03/2026

# The purpose of this file is to subset spring and CBC data to only species
# in mBBS. Additionally, create heatmaps in the same order

library(ggplot2)
library(corrplot)
library(png)
library(pheatmap)
library(correlation)
library(dplyr)
library(sna)
library(tidyverse)
library(patchwork)

# Read in data, filter to only mbbs species and drop year column
cbc <- read.csv("data/CBCHistoricData/CBCMERGEDDeltaY.csv") |> select(-year)
spring <- read.csv("data/Spring/SpringDeltaYWide.csv")|> select(-year)
mbbs <- read.csv("data/mbbs/mbbsDeltaYWide.csv")|> select(-year)
temp <- intersect(names(spring), intersect(names(cbc), names(mbbs)))

cbc <- cbc |> select(any_of(temp))
spring <- spring |> select(any_of(temp))
mbbs <- mbbs |> select(any_of(temp))

#Gets row/col order based on mbbs
cor_matrixMbbs <- cor(mbbs, method = "spearman")
temp1 <- pheatmap(cor_matrixMbbs, color = hcl.colors(50, "RdBu"), main = "mBBS",
                  breaks = seq(-1, 1, by = 0.04))
rowOrder <- temp1$tree_row$order
colOrder <- temp1$tree_col$order

createCorrMatrixSubset <- function(survey, heatmapName, sizeWH, title){
  corMatrix <- cor(survey, method = "spearman")
  #Creates visual of heatmap
  png(filename = paste("figures/heatmap/subsets/", heatmapName, ".png", sep = ""), width = sizeWH, height = sizeWH)
  pheatmap(corMatrix[rowOrder, colOrder], cluster_rows = FALSE, cluster_cols = FALSE,
           color = hcl.colors(50, "RdBu"), 
           main = title,
           breaks = seq(-1, 1, by = 0.04))
  dev.off()
  
  sorting_columns <- c("sp1", "sp2")
  
  corMatrix <- as.data.frame(corMatrix)
  readable_corr <- corMatrix |>
    mutate(sp2 = rownames(corMatrix)) |>
    pivot_longer(col = 0:(ncol(corMatrix)),
                 names_to = "sp1", values_to = "corr") |>
    #remove duplicates
    filter(!(sp1 == sp2))
  
  readable_corr <- readable_corr[!duplicated(apply(readable_corr[,1:2], 1, function(row) paste(sort(row), collapse=""))),]
  
  write.csv(readable_corr, file = paste("data/corrMatrices/subsets/", heatmapName, ".csv", sep = ""), row.names = TRUE)
  return(readable_corr)
}

cbcMatrix <- createCorrMatrixSubset(cbc, "CBCSubset", 600, "CBC - Subset Yearly Abundance Change Matrix")
mbbsMatrix <- createCorrMatrixSubset(mbbs, "mBBSSubset", 600, "mBBS - Subset Yearly Abundance Change Matrix")
springMatrix <- createCorrMatrixSubset(spring, "SpringSubset", 600, "Spring - Subset to mBBS Yearly Abundance Change Matrix")





