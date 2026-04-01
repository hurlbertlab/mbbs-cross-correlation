# Created 02/16/2026
# Last updated: 02/16/2026 - Anneliese Pinnell

# The goal of this code is to create a file that runs all necessary
# commands on one file instead of different places
options(warn = -1)

## Overall creates correlation heatmaps and corr matricies
source("code/corr/0CBCtoCorrectFormat.R") # Tested
source("code/corr/0cleanCBCNames.R") # Tested
source("code/corr/1predictMissingMBBS.R") # Tested
source("code/corr/1combineMBBSAndPredicted.R") # Tested
source("code/corr/2createWideLong.R") # Tested
source("code/corr/3makeCorrMatrix.R") # Tested

#Gets relevant species from BirdBase and AvoNet
source("code/traits/0spInterest.R")
#source("code/traits/1selfCalculations.R") #Not using
source("code/traits/1manipulateTraits.R")
source("code/traits/2selfGraphs.R")

#Get range and daymet data
source("code/rangeTemp/1getEBirdRanges.R")
source("code/rangeTemp/1getNorthAmerica.R")
source("code/rangeTemp/2DAYMETAccess.R")



