# Read in trait correlations and make a nice table
library(gt)
library(tidyverse)

mbbs <- read.csv("data/variableInfluences/mBBSValues.csv") |>
  mutate(survey = "MBBS", .before = 3)
cbc <- read.csv("data/variableInfluences/CBCValues.csv") |>
  mutate(survey = "CBC", .before = 3)
mbbsRes <- read.csv("data/variableInfluences/mBBSResValues.csv")
cbcRes <- read.csv("data/variableInfluences/CBCResValues.csv")

#mbbsAndCBC <- bind_rows(mbbs, cbc)
mbbsAndCBC <- left_join(mbbs, cbc, by = c("X", "variable_name"))

allSurveys <- list(mbbs, cbc)#, mbbsRes, cbcRes)
#firstColumn <- mbbs$X

firstColumn <- c("Absolute Mass Difference", "Absolute Migration Difference", "Identical Trophic Level", 
                 "Identical Primary Habitat", "Absolute Habitat Density Difference", "Absolute Diet Breadth Difference")
surveys <- c("MBBS", "CBC") #"mBBS Residents", "CBC Residents")
for(i in seq(length(allSurveys)-1)){
  table <- data.frame(a = firstColumn,
                      b = surveys[[i]],
                      c = round(allSurveys[[i]]$coefficient, 3),
                      d = round(allSurveys[[i]]$p_value, 3),
                      e = surveys[[i+1]],
                      f = round(allSurveys[[i+1]]$coefficient, 3),
                      g = round(allSurveys[[i+1]]$p_value, 3))
  table <- gt(table) |>
    cols_label(
      a = "Trait Name",
      b = "Survey",
      c = "Coefficient",
      d = "p-value",
      e = "Survey",
      f = "Coefficient",
      g = "p-value"
    )
  
  table <- table |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = d,
        rows = d < 0.05
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = g,
        rows = g < 0.05
      )
    )
  
  display_table <- table
  png(filename = paste("figures/variableInfluence/", as.character(i), ".png"),
      width = 8, height = 2, units = "in", 
      res = 300)
  plot(display_table)
  dev.off()
}


