# 04/16/2026
# Anneliese Pinnell

#Filter big correlation matrix to be smaller and include values over a certain 
# threshold

cbcMatrix <- read.csv("data/corrMatrices/cbc_delta_y_corr_matrix.csv") 

getHighOnly <- function(matrix, name_png, strength){
  #Must be high in general
  getHighCorrSpecies <- matrix |>
    pivot_longer(cols = -X,
                 names_to = "sp2",
                 values_to = "corr") |>
    filter(corr != 1 & corr >= strength) |>
    distinct(X)
  
  #Subset to only high in general
  onlyHighCorr <- matrix |>
    filter(X %in% c(getHighCorrSpecies$X)) |>
    select(any_of(c(getHighCorrSpecies$X))) |>
    mutate(X = c(getHighCorrSpecies$X), .before = 1) |>
    column_to_rownames(var = "X") |>
    as.matrix()
    
  png(filename = paste("figures/heatmap/importantSubset/", name_png, ".png", sep = ""), 
      width = 10, height = 10, units = "in", 
      res = 300)
  pheatmap(onlyHighCorr, color = hcl.colors(50, "RdBu"),
           breaks = seq(-1, 1, by = 0.04), 
           cluster_cols = TRUE, cluster_rows = TRUE, fontsize = 15
  )
  dev.off()
}

#For CBC
getHighOnly(matrix = read.csv("data/corrMatrices/cbc_delta_y_corr_matrix.csv"),
            name_png = "CBC",
            strength = 0.61) #Ensures Golden-crowned Kinglet is kept

#For mBBS
getHighOnly(matrix = read.csv("data/corrMatrices/mbbs_delta_y_corr_matrix.csv"),
            name_png = "mBBS",
            strength = 0.52) #Ensures Blue Grosbeak is kept
