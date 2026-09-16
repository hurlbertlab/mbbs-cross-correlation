library(tidyverse)
library(ggplot2)
library(gridExtra)

mbbs <- read.csv("data/residents/mbbsDeltaYLong.csv")
species <- c(unique(mbbs$common_name))
species <- "Northern Bobwhite"
# Residents only
allTogether <- data.frame(common_name = species)
lags <- c(seq(1, 7))

for(i in seq_along(lags)){
  mbbs <- read.csv("data/residents/mbbsDeltaYLong.csv") |>
    filter(common_name %in% species) |>
    mutate(mbbsIncreasing = case_when(
      yoy_change >= 0 ~ 1,
      yoy_change < 0 ~ -1,
      is.na(yoy_change) ~ FALSE
    ))|>
    mutate(year = year - i) 
  |>
    select(-c(yoy_change))
  
  cbc <- read.csv("data/residents/cbcDeltaYLong.csv") |>
    filter(common_name %in% species) |>
    mutate(cbcIncreasing = case_when(
      yoy_change >= 0 ~ 1,
      yoy_change < 0 ~ -1,
      is.na(yoy_change) ~ FALSE
    )) 
  |>
    select(-c(yoy_change))
  
  joined <- left_join(mbbs, cbc, by = c("year", "common_name")) |>
    drop_na() |>
    mutate(matching = if_else(mbbsIncreasing == cbcIncreasing, "Match", "Mismatched"))
  
  temp <- joined |>
    group_by(common_name) |>
    summarize(totalMatching = sum(matching == "Match", na.rm = TRUE))
  
  joined <- left_join(joined, temp, by = "common_name") |>
    arrange(totalMatching)
  
  forTable <- joined |>
    select(c(common_name, totalMatching)) |>
    unique()
  names(forTable) <- c("common_name", as.character((i-1)))
  
  allTogether <- left_join(allTogether, forTable, by = "common_name")
}

png("figures/matchingDeltas.png", height = 800, width = 400)
grid.table(allTogether, rows = NULL)
dev.off()

# Plotting
ggplot(joined, aes(x = year, y = fct_reorder(common_name, totalMatching), fill = matching)) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_manual(values = c("Match" = "#4CAF50", "Mismatched" = "#F44336")) +
  labs(
    title = "Trend Match Comparison by Species Over Time",
    x = "Year",
    y = "Species",
    fill = "Status"
  ) +
  theme_minimal()
