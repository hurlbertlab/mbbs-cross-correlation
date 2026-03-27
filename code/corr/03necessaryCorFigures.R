# Created 03/25/2026

library(patchwork)
library(ggplot2)
library(dplyr)
# Put in necessary graphs for mbbs and cbc data
init_par <- par(no.readonly = TRUE) 

cbc <- read.csv("data/CBCHistoricData/CBCDeltaYLong.csv")
mbbs <- read.csv("data/mbbs/mbbsDeltaYLong.csv")

figure <- ggplot(cbc, aes(x=yoy_change)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  labs(x="Abundance per Effort Hour",
       y = "Density",
       title = "CBC - Abundance per Effort Hour") +
  geom_density(alpha=.2, fill="#FF6666")

figure2 <- ggplot(mbbs, aes(x=yoy_change)) + 
  geom_histogram(aes(y=after_stat(density)), colour="black", fill="white") +
  labs(x="Abundance per Effort Hour",
       y = "Density",
       title = "mBBS - Abundance per Effort Hour") +
  geom_density(alpha=.2, fill="#FF6666")


png(filename = "figures/abundanceByEffortHr.png", width = 6, height = 6,
    units = "in", res = 300)
figure/figure2
dev.off()

cbc2 <- cbc |>
  mutate(yoy_change = abs(yoy_change)) |>
  arrange(desc(yoy_change)) |>
  slice(1:30)

c(unique(cbc2$common_name))

mbbs2 <- mbbs |>
  mutate(yoy_change = abs(yoy_change)) |>
  arrange(desc(yoy_change)) |>
  slice(1:20)

c(unique(mbbs2$common_name))
