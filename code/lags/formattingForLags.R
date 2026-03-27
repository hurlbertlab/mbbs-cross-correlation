# Created 3/26/2026 by Anneliese Pinnell
# The goal of this file is to format mBBS and CBC to be usable for lags
library(tidyverse)
library(ggplot2)
library(corrplot)
library(beepr)

# mbbs = 1 and year ends in 0.65
# winter = 2 and year ends in 0.95

cbc <- read.csv("data/CBCHistoricData/CBCDeltaYLong.csv") |>
  mutate(season = 2) |>
  mutate(seasonalYear = year+0.95) |>
  filter(year <= 2002)
mbbs <- read.csv("data/mbbs/mbbsDeltaYLong.csv") |>
  mutate(season = 1) |>
  mutate(seasonalYear = year+0.65) |>
  filter(year <= 2002)

both <- intersect(cbc$common_name, mbbs$common_name)
cbc <- cbc |>
  filter(common_name %in% both)
mbbs <- mbbs |>
  filter(common_name %in% both)

joined <- bind_rows(cbc, mbbs) |>
  arrange(seasonalYear, season) |>
  group_by(common_name)|>
  nest()

#Calculates delta y
joined$data <- map(joined$data, ~ {
  diffVec <- c(.x$yoy_change[1], diff(.x$yoy_change))
  .x |>
    mutate(ongoing_lag = diffVec)
})

#unnests data
seasonalChange <- joined |>
  unnest(data)

#plot(seasonalChange$seasonalYear, seasonalChange$ongoing_lag, col = seasonalChange$season)
#lines(seasonalChange$seasonalYear, seasonalChange$ongoing_lag)


# Now you need to get correlation values between species over 1 year
# sp1 and sp2
pairs <- as.data.frame(t(combn(both, 2)))|>
  slice(1:10)

lagCorrelation <- data.frame("sp1" = c(pairs$V1), "sp2" = c(pairs$V2))

allCorrs <- c()
for(i in seq(length(lagCorrelation$sp1))){
  sp1Data <- seasonalChange |>
    filter(common_name == lagCorrelation$sp1[i])
  sp2Data <- seasonalChange |>
    filter(common_name == lagCorrelation$sp2[i])
  corrValue <- cor(sp1Data$ongoing_lag, sp2Data$ongoing_lag)
  allCorrs <- c(allCorrs, corrValue)
}

lagCorrelation$corr <- allCorrs
beepr::beep(4)
