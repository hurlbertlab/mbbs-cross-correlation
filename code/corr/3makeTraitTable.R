# Read in trait correlations and make a nice table
library(gt)

mbbs <- read.csv("data/variableInfluences/mBBSValues.csv")
cbc <- read.csv("data/variableInfluences/CBCValues.csv")
mbbsRes <- read.csv("data/variableInfluences/mBBSResValues.csv")
cbcRes <- read.csv("data/variableInfluences/CBCResValues.csv")

allSurveys <- list(mbbs, cbc, mbbsRes, cbcRes)
#firstColumn <- mbbs$X

firstColumn <- c("Absolute Mass Difference", "Absolute Migration Difference", "Identical Trophic Level", 
                 "Identical Primary Habitat", "Absolute Habitat Density Difference", "Absolute Diet Breadth Difference")
surveys <- c("mBBS", "CBC", "mBBS Residents", "CBC Residents")
for(i in seq(length(allSurveys))){
  table <- data.frame("Trait Name" = firstColumn,
                      "Survey" = surveys[[i]],
                      "Coefficient" = round(allSurveys[[i]]$coefficient, 3),
                      "p-value" = round(allSurveys[[i]]$p_value, 3))
  
  display_table <- gt(table)
  png(filename = paste("figures/variableInfluence/", as.character(i), ".png"),
      width = 6, height = 2, units = "in", 
      res = 300)
  plot(display_table)
  dev.off()
}


