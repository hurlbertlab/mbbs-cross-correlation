# Created 02/16/2026
# Last updated: 02/16/2026 - Anneliese Pinnell

# The goal of this code is to create a file that runs all necessary
# commands on one file instead of different places

## Overall creates correlation heatmaps and corr matricies
source("code/corr/CBCtoCorrectFormat.R")
source("code/corr/cleanCBCNames.R")
source("code/corr/SpringToCorrectFormat.R")
source("code/corr/predictMissingMBBS.R")
source("code/corr/combineMBBSAndPredicted.R")
source("code/corr/createWideLong.R")
source("code/corr/makeCorrMatrix.R")

#Gets relevant species from BirdBase and AvoNet
source("code/spInterest.R")



