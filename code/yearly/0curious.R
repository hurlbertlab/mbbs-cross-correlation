library(tidyverse)
library(ggplot2)
library(gridExtra)

mbbs <- read.csv("data/mbbs/mbbsDeltaYLong.csv")
cbc <- read.csv("data/CBCHistoricData/CBCDeltaYLong.csv")
spring <- read.csv("data/Spring/SpringDeltaYLong.csv")
species <- intersect(intersect(mbbs$common_name, cbc$common_name), spring$common_name)

# Residents only
allTogether <- data.frame(common_name = species)
lags <- c(1)
percentage <- 1.10
for(i in seq_along(lags)){
  tempMbbs <- mbbs |>
    filter(common_name %in% species) |>
    group_by(common_name) |>
    mutate(mbbsIncreasing = case_when(
      yoy_change >= percentage*lag(yoy_change, n = 1) ~ 1,
      yoy_change <= percentage*lag(yoy_change, n = 1) ~ -1,
      yoy_change  > -100000~ 0,
      is.na(yoy_change) ~ FALSE
    ))|>
    mutate(year = year - i) |>
    select(-c(yoy_change))
  
  tempCBC <- cbc |>
    filter(common_name %in% species) |>
    group_by(common_name) |>
    mutate(cbcIncreasing = case_when(
      yoy_change >= percentage*lag(yoy_change, n = 1) ~ 1,
      yoy_change <= percentage*lag(yoy_change, n = 1) ~ -1,
      yoy_change  > -100000~ 0,
      is.na(yoy_change) ~ FALSE
    ))|>
    select(-c(yoy_change))
  
  tempSpring <- spring |>
    filter(common_name %in% species) |>
    group_by(common_name) |>
    mutate(springIncreasing = case_when(
      yoy_change >= percentage*lag(yoy_change, n = 1) ~ 1,
      yoy_change <= percentage*lag(yoy_change, n = 1) ~ -1,
      yoy_change  > -100000~ 0,
      is.na(yoy_change) ~ FALSE
    ))|>
    mutate(year = year - i) |>
    select(-c(yoy_change))
  
  joined <- left_join(left_join(tempMbbs, tempCBC, by = c("year", "common_name")),
                      tempSpring, by = c("year", "common_name")) |>
    drop_na() |>
    mutate(MC = if_else(mbbsIncreasing == cbcIncreasing, "Match", "Mismatched")) |>
    mutate(MS = if_else(mbbsIncreasing == springIncreasing, "Match", "Mismatched")) |>
    mutate(CS = if_else(cbcIncreasing == springIncreasing, "Match", "Mismatched")) |>
    mutate(generalMatch =if_else(((MC == MS) & (MS == CS)), "Match", "Mismatched")) |>
    mutate(seasonalMatch = case_when(
      generalMatch == "Match" ~ "All seasons match",
      MC == "Match" ~ "MBBS and CBC match",
      MS == "Match" ~ "MBBS and Spring match",
      CS == "Match" ~ "CBC and Spring match",
      TRUE ~ "Exception found"
      ))
  
  temp <- joined |>
    group_by(common_name) |>
    summarize(totalMatching = sum(generalMatch == "Match", na.rm = TRUE))
  
  joined <- left_join(joined, temp, by = "common_name") |>
    arrange(totalMatching)
  
  forTable <- joined |>
    select(c(common_name, totalMatching)) |>
    unique()
  names(forTable) <- c("common_name", as.character((i-1)))

allTogether <- left_join(allTogether, forTable, by = "common_name")
}

png("figures/SpringMatchingDeltas.png", height = 800, width = 400)
grid.table(allTogether, rows = NULL)
dev.off()


columns <- c("MC", "MS", "CS")
names <- c("MBBS/CBC", "MBBS/Spring", "CBC/Spring")
plotList <- list()
for(i in seq_along(columns)){
  temp <- joined |>
    group_by(common_name) |>
    summarize(totalMatching = sum(.data[[columns[i]]] == "Match", na.rm = TRUE), .groups = "drop")
  joined <- left_join(joined, temp, by = c("common_name", "totalMatching")) |>
    arrange(totalMatching)
  
  p <- ggplot(joined, aes(x = year, y = fct_reorder(common_name, .data[[columns[i]]]), fill = .data[[columns[i]]])) +
    geom_tile(color = "white", linewidth = 0.6) +
    scale_fill_manual(values = c("Match" = "navy", 
                                 "Mismatched" = "darkorange")) +
    labs(
      title = paste0(names[i], " Match Comparison by Species Over Time"),
      x = "Year",
      y = "Species",
      fill = "Status"
    ) +
    theme_minimal()
  
  png(paste0("figures/matchingTrends/", columns[i], ".png"), units = "in",
      height = 5, width = 8, res = 300)
  plot(p)
  dev.off()
}

# Plotting
