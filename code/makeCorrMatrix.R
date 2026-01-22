# Created 11/20/2025
# Last updated 11/20/2025 by Anneliese Pinnell

#This file takes the wide form mbbs data and creates a correlation matrix.
#Also exports sorted heatmap matrix to png and exports sorted matrix as csv.

library(ggplot2)
library(corrplot)
library(png)


createMatrixPlot <- function(fileName, name_png, title, width, fontSize, titleSize){
  wide_form_data = read.csv(fileName)
  #Correlation matrix
  cor_matrix <- cor(wide_form_data)
  #Drop the year column (first column)
  cor_matrix <- cor_matrix[, -1]
  #Drop the year row (first row)
  cor_matrix <- cor_matrix[-1, ]
  
  #Opening a PNG device
  png(filename = paste("figures/", name_png, ".png", sep = ""), width = width, height = width)
  cor_matrix <- corrplot(cor_matrix, order="hclust", tl.col = "black", tl.cex = fontSize, cl.cex = fontSize*1.5)
  title(main=title, cex.main = titleSize)
  dev.off()
  
  #Getting sorted order from corrplot output
  sorted_corr <- cor_matrix$corr
  #Exporting sorted matrix as csv file
  sorted_corr <- as.data.frame(sorted_corr)
  write.csv(sorted_corr, file = paste("data/corrMatrices/", name_png, ".csv", sep = ""), row.names = TRUE)
  
}

#Run for CBC Delta Y
createMatrixPlot("data/CBCHistoricData/CBCMergedDeltaY.csv",
                 "r_cbc_delta_y_corr_matrix","CBC Delta Y Correlation Matrix (1902/1977/1923-2024)", 
                 3000, 2, 5)

#Run for CBC non-Delta Y
createMatrixPlot("data/CBCHistoricData/CBCMergedWide.csv",
                 "r_cbc_corr_matrix","CBC Correlation Matrix (1902/1977/1923-2024)", 
                 3000, 2, 5)

#Run for mBBS Delta Y
createMatrixPlot("data/mbbs/mbbsDeltaYWide.csv",
                 "mbbs_corr_matrix","mBBS Delta Y Correlation Matrix (1999-2024)",
                 1200, 1, 2)

#Run for mBBS non-Delta Y
createMatrixPlot("data/mbbs/mbbsWide.csv",
                 "mbbs_delta_y_corr_matrix","mBBS Correlation Matrix (1999-2024)", 
                 1200, 1, 2)

#Run for Spring 
createMatrixPlot("data/Spring/SpringWide.csv",
                 "spring_corr_matrix","Spring Correlation Matrix (-2024)", 
                 3000, 2, 5)

#Run for Spring delta Y
createMatrixPlot("data/Spring/SpringDeltaYWide.csv",
                 "spring_delta_y_corr_matrix","Spring Delta Y Correlation Matrix (-2024)", 
                 3000, 2, 5)

