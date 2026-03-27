# Created 02/10/2026
# Last updated 02/10/2026 by Anneliese Pinnell

# The purpose of this file is to merge create a dendrogram from the data 
# derived from the correlation matrices

createDendr <- function(fileName, figureName, figureFileName){
  correPlot <- read.csv(fileName, row.names = 1)
  #Convert correlation matrix to distance
  distance <- dist(1-correPlot)
  
  #Hierarchical clustering
  hcluster <- hclust(distance, method = "complete")
  
  #Create a dendrogram
  dendrogram <- as.dendrogram(hcluster)
  
  png(figureFileName, width = 600, height = 1000)
  #Plot dendrogram
  par(mar=c(2,4,2,10))
  plot(dendrogram, main = figureName, horiz=TRUE)
  dev.off()
}

# mBBS
createDendr("data/corrMatrices/mbbs_delta_y_corr_matrix.csv",
            "Dendrogram from mBBS Delta Y (1999-2025)",
            "figures/dendr/mBBSDendrogram.png")
# Spring
createDendr("data/corrMatrices/spring_delta_y_corr_matrix.csv",
            "Dendrogram from Spring Delta Y (1999-2025)",
            "figures/dendr/springDendrogram.png")
# CBC
createDendr("data/corrMatrices/cbc_delta_y_corr_matrix.csv",
            "Dendrogram from CBC Delta Y (1999-2025)",
            "figures/dendr/cbcDendrogram.png")
