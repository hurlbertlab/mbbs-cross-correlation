# Created 11/20/2025
# Last updated 11/28/2025 by Anneliese Pinnell
  # Added pct change stuff, and seperate sig and insig
  # 02/10/2026 Renamed output files for CBC

#This file takes the wide form mbbs data and creates a correlation matrix.
#Also exports sorted heatmap matrix to png and exports sorted matrix as csv.

library(ggplot2)
library(corrplot)
library(png)
library(pheatmap)

createMatrixPlot <- function(fileName, name_png, title, width, fontSize, titleSize){
  wide_form_data = read.csv(fileName)
  #Correlation matrix
  cor_matrix <- cor(wide_form_data, method = "spearman")
  #cor_matrix <- cor(wide_form_data)
  #Drop the year column (first column)
  cor_matrix <- cor_matrix[, -1]
  #Drop the year row (first row)
  cor_matrix <- cor_matrix[-1, ]
  
  #Creates corrplot figure with insig values
  #Opening a PNG device
  png(filename = paste("figures/heatmap/", name_png, ".png", sep = ""), width = width, height = width)
  pheatmap(cor_matrix, color = hcl.colors(50, "RdBu"))
  dev.off()
  
  sorted_corr <- cor_matrix_plot$corr
  #Exporting sorted matrix as csv file
  sorted_corr <- as.data.frame(sorted_corr)
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


