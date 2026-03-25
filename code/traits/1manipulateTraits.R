# Last updated: 02/18/2026 - Anneliese Pinnell
  # 03/02/2026 - switched to difference for migration and log diffs
# Attempt for all


library(tidyverse)


createTraitsAndCorr <- function(traitsFile, corrFile, outFileName, labelLength){
  #Load in data
  mbbsTraits <- read.csv(traitsFile) %>%
    #create an easier to use common name column
    mutate(common_name = English.Name..BirdLife...IOC...Clements.AviList.)
  
  mbbsCorr <- read.csv(corrFile)
  
  #Converts characters to factors
  mbbsTraits[sapply(mbbsTraits, is.character)] <- lapply(mbbsTraits[sapply(mbbsTraits, is.character)], as.factor)
  
  #Everyone together!
  together <- mbbsTraits
  
  # Remove columns with at least one NA
  together <- together[, colSums(is.na(together)) == 0]
  
  #Get the number of unique species
  n_species <- length(unique(together$common_name))
  #Get the unique species
  species_list <- unique(together$common_name)
  
  sp1 <- rep(species_list, each = n_species)
  sp2 <- rep(species_list, times = n_species)
  
  calculated <- data.frame(sp1 = sp1, sp2 = sp2) |>
    left_join(together, by = c("sp1" = "common_name")) |>
    left_join(together, by = c("sp2" = "common_name")) |> 
    mutate(dif_mig = log(Migration.x/Migration.y),
           dif_habDen = log(Habitat.Density.x/Habitat.Density.y),
           dif_mass = log(Mass.x/Mass.y),
           dif_tailLen = log(Tail.Length.x/Tail.Length.y),
           dif_handWing = log(Hand.Wing.Index.x/Hand.Wing.Index.y),
           dif_Secondary = log(Secondary1.x/Secondary1.y),
           dif_KippsDist = log(Kipps.Distance.x/Kipps.Distance.y),
           dif_wingLen = log(Wing.Length.x/Wing.Length.y),
           dif_tarsusLen = log(Tarsus.Length.x/Tarsus.Length.y),
           dif_beakDep = log(Beak.Depth.x/Beak.Depth.y),
           dif_width = log(Beak.Width.x/Beak.Width.y),
           dif_beakLenNare = log(Beak.Length_Nares.x/Beak.Length_Nares.y),
           dif_beakLenCulm = log(Beak.Length_Culmen.x/Beak.Length_Culmen.y), #End of AvoNet
           dif_clutch = log((Clutch_Max.x - Clutch_Min.x)/(Clutch_Max.y - Clutch_Min.y)),
           #dif_clutchMin = log(Clutch_Min.x/Clutch_Min.y),
           #dif_clutchMax = log(Clutch_Max.x/Clutch_Max.y),
           dif_ESI = log(ESI.x/ESI.y),
           dif_DB = log(DB.x/DB.y),
           dif_HB = log(HB.x/HB.y),
           dif_normMax = log(NormMax.x/NormMax.y),
           dif_elevaRange = log(Elevational.Range.x/Elevational.Range.y),
           dif_weight = log(Average.Mass.x/Average.Mass.y),
           dif_LAT = log(LAT.x/LAT.y),
           dif_migDist = log(migDistanceKM.x/migDistanceKM.y),
           dif_arth = log(Final_Fraction_Diet_Wt.x/Final_Fraction_Diet_Wt.y)
           ) |>
    mutate(dif_RLM = RLM.x == RLM.y,
           dif_primHab = Primary.Habitat.x == Primary.Habitat.y,
           dif_primDiet = Primary.Diet.x == Primary.Diet.y,
           dif_INwt = IN.Wt.x == IN.Wt.y,
           dif_FRwt = FR.Wt.x == FR.Wt.y,
           dif_NEwt = NE.Wt.x == NE.Wt.y,
           dif_SEwt = SE.Wt.x == SE.Wt.y,
           dif_VEwt = VE.Wt.x == VE.Wt.y,
           dif_FIwt = FI.Wt.x == FI.Wt.y,
           dif_SCwt = SC.Wt.x == SC.Wt.y,
           dif_PLwt = PL.Wt.x == PL.Wt.y,
           dif_MSwt = MS.Wt.x == MS.Wt.y,
           #dif_NestType = Nest_Type.x == Nest_Type.y,
           #dif_NestSBS = Nest_SBS.x == Nest_SBS.y,
           #dif_incuSex = Incu_Sex.x == Incu_Sex.y,
           dif_trophicLvl = Trophic.Level.x == Trophic.Level.y,
           dif_trophicNic = Trophic.Niche.x == Trophic.Niche.y,
           dif_primLife = Primary.Lifestyle.x == Primary.Lifestyle.y
    ) |>
    mutate(across(where(is.logical), as.numeric)) #converts True --> 1 and False --> 0
  
  #Corrects name format for acessing correlation values
  calculated$sp1 <- gsub(" |-|\'", ".", calculated$sp1)
  calculated$sp2 <- gsub(" |-|\'", ".", calculated$sp2)
  
  #Removes duplicates
  calculated <- calculated[!duplicated(apply(calculated[,1:2], 1, function(row) paste(sort(row), collapse=""))),]
  
  #Accesses correlation values for species
  corrValues <- c()
  for (i in 1:length(calculated$sp1)){
    addedValue <- mbbsCorr |> 
      filter(X == calculated$sp1[i]) |> 
      select(calculated$sp2[i])
    corrValues <- c(corrValues, addedValue)
  }
  #Creates a new column in calcualted with corresponding correlation values
  calculated$corr <- as.numeric(corrValues)
  
  cleaned <- calculated |>
    select(contains("dif_"))
  
  cleaned$sp1 <- calculated$sp1
  cleaned$sp2 <- calculated$sp2
  cleaned$corr <- calculated$corr
  cleaned <- cleaned |>
    mutate(across(where(is.numeric), ~ifelse(is.infinite(.), 0, .)))
  
  write.csv(cleaned, outFileName)
}

# mbbs
createTraitsAndCorr("data/traits/mbbsTraits.csv", 
                    "data/corrMatrices/mbbs_delta_y_corr_matrix.csv",
                    "data/traits/mbbsTraitsAndCorr.csv", 8)

# CBC
createTraitsAndCorr("data/traits/CBCTraits.csv", 
                    "data/corrMatrices/cbc_delta_y_corr_matrix.csv",
                    "data/traits/CBCTraitsAndCorr.csv", 7)
