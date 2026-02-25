# Created 02/25/2026

# The purpose of this file is to establish if there is a period of time 
# by years that correlation becomes highest

library(correlation)

residents <- read.csv("data/residents/deltaYChangeWide.csv")

iterations <- 1:26

allCorrMatricies <- data.frame(year = c(), data = list(data.frame()), mean = c())

#Creates nested dataframes for each year
for (i in iterations){
  years <- 1999:(1999+i)
  
  specificResidents <- residents |>
    filter(year %in% years)
  
  #Drop year column
  wide_form_data <- specificResidents[, -1]
  #Correlation matrix
  cor_matrix <- cor(wide_form_data, method = "spearman")
  
  sorted_corr <- as.data.frame(cor_sort(cor_matrix))
  
  toPass <- data.frame(data = list(sorted_corr))
  new_row <- tibble(id = i, data = list(sorted_corr), mean = mean(cor_matrix))
  
  allCorrMatricies <- rbind(allCorrMatricies, new_row)
}

#Repalces ID column with year
allCorrMatricies$id <- 1999:2024




