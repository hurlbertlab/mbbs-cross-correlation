# Created 11/20/2025
# Last updated 11/28/2025 by Anneliese Pinnell
# Added pct change stuff, and seperate sig and insig
# 02/10/2026 Renamed output files for CBC
# ~2/14/2026 Changed from heatmap --> pheatmap and sorted cor plot 
# using correlation library
# 2/18/2026 made script shorter by removing year column earlier

#This file takes the wide form mbbs data and creates a correlation matrix.
#Also exports sorted heatmap matrix to png and exports sorted matrix as csv.

library(ggplot2)
library(corrplot)
library(png)
library(pheatmap)
library(correlation)

createMatrixPlot <- function(fileName, name_png, title, width, fontSize, titleSize){
  wide_form_data = read.csv(fileName) |>
    mutate(year = NULL) #gets rid of year column if it exists, and if it doesn't exist does nothing

  #Correlation matrix
  cor_matrix <- cor(wide_form_data, method = "spearman")
  
  #Creates corrplot figure with insig values
  #Opening a PNG device
  png(filename = paste("figures/heatmap/", name_png, ".png", sep = ""), width = width, height = width)
  pheatmap(cor_matrix, color = hcl.colors(50, "RdBu"), main = title,
           breaks = seq(-1, 1, by = 0.04))
  dev.off()
  
  sorted_corr <- cor_sort(cor_matrix)
  #Exporting sorted matrix as csv file
  sorted_corr <- as.data.frame(sorted_corr)
  
  sorting_columns <- c("sp1", "sp2")
  
  readable_corr <- sorted_corr |>
    mutate(sp2 = rownames(sorted_corr)) |>
    pivot_longer(col = 0:(ncol(sorted_corr)),
                 names_to = "sp1", values_to = "cor") |>
    #remove duplicates
    filter(!(sp1 == sp2))
    
  readable_corr <- readable_corr[!duplicated(apply(readable_corr[,1:2], 1, function(row) paste(sort(row), collapse=""))),]
  
  write.csv(sorted_corr, file = paste("data/corrMatrices/", name_png, ".csv", sep = ""), row.names = TRUE)
  
}


#Run for CBC Delta Y
createMatrixPlot("data/CBCHistoricData/CBCMergedDeltaY.csv",
                 "cbc_delta_y_corr_matrix","CBC Delta Y Correlation Matrix (1999-2025) By Effort Hour", 
                 900, 2, 5)

#Run for CBC non-Delta Y
createMatrixPlot("data/CBCHistoricData/CBCMergedWide.csv",
                 "cbc_corr_matrix","CBC Correlation Matrix (1999-2025) By Effort Hour", 
                 900, 2, 5)


#Run for mBBS Delta Y
createMatrixPlot("data/mbbs/mbbsDeltaYWide.csv",
                 "mbbs_delta_y_corr_matrix","mBBS Delta Y Correlation Matrix (1999-2025)",
                 600, 1, 2)

#Run for mBBS non-Delta Y
createMatrixPlot("data/mbbs/mbbsWide.csv",
                 "mbbs_corr_matrix","mBBS Correlation Matrix (1999-2025)", 
                 600, 1, 2)

#Run for Spring 
createMatrixPlot("data/Spring/SpringWide.csv",
                 "spring_corr_matrix","Spring Correlation Matrix (1999-2025)", 
                 900, 2, 5)

#Run for Spring delta Y
createMatrixPlot("data/Spring/SpringDeltaYWide.csv",
                 "spring_delta_y_corr_matrix","Spring Delta Y Correlation Matrix (1999-2025)", 
                 900, 2, 5)

#Run for residents delta Y
createMatrixPlot("data/residents/deltaYChangeWide.csv",
                 "residents_delta_y_corr_matrix","Residents Seasonal Difference Delta Y Correlation Matrix (1999-2025)", 
                 600, 2, 5)

#Run for residents
createMatrixPlot("data/residents/residentSpeciesWide.csv",
                 "residents_corr_matrix","Residents Seasonal Change Correlation Matrix (1999-2025)", 
                 600, 2, 5)


