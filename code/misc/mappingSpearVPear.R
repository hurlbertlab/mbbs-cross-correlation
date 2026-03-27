# Created 02/12/2026
# Last updated 02/12/2026 by Anneliese Pinnell

# This file maps pearson vs. spearman correlations for species and population
# fluctuations

library(tidyverse)
library(ggplot2)

mbbs <- read.csv("data/mbbs/mbbsLong.csv")

birdsOfInterest <- c("Carolina Chickadee", "Tufted Titmouse", "Pine Warbler",
                     "Eastern Towhee", "Barn Swallow", "White-breasted Nuthatch",
                     "American Goldfinch", "Brown-headed Cowbird")
#Birds of interest
specficBirds <- mbbs |>
  filter(common_name %in% birdsOfInterest) |>
  group_by(common_name, year) |>
  mutate(count = sum(count)) |>
  unique()


# Sample data
chick <- specficBirds |> filter(common_name == "Carolina Chickadee")
tuft <- specficBirds |> filter(common_name == "Tufted Titmouse")
pine <- specficBirds |> filter(common_name == "Pine Warbler")
towhee <- specficBirds |> filter(common_name == "Eastern Towhee")
barn <- specficBirds |> filter(common_name == "Barn Swallow")
wnut <- specficBirds |> filter(common_name == "White-breasted Nuthatch")
gold <- specficBirds |> filter(common_name == "American Goldfinch")
brown <- specficBirds |> filter(common_name == "Brown-headed Cowbird")

birds <- list(chick, tuft, pine, towhee, barn, wnut, gold, brown)
png(filename = paste("figures/", "svpALL", ".png", sep = ""), 
    width = 1000, height = 1000)
par(mfrow=c(4,2))
numbers <- seq(2, length(birds), 2)

for (item in numbers){
  lreg <- lm(rank(birds[[item-1]]$count) ~ rank(birds[[item]]$count))
  #Spearman
  plot(rank(birds[[item]]$count), rank(birds[[item-1]]$count), main="Spearman Correlation - mbbs", 
       xlab=birdsOfInterest[[item]], ylab=birdsOfInterest[[item-1]], cex.lab=1.5,
       cex.main = 2)
  abline(lreg, col = "red", lwd=2)
  r <- round(cor(rank(birds[[item-1]]$count), fitted(lreg)), 4)
  mtext(r, side=3)
  
  lreg2 <- lm(birds[[item-1]]$count ~ birds[[item]]$count)
  #Pearson
  plot(birds[[item]]$count, birds[[item-1]]$count, main="Pearson Correlation - mbbs", 
       xlab=birdsOfInterest[[item]], ylab=birdsOfInterest[[item-1]], cex.lab=1.5,
       cex.main = 2)
  abline(lreg2, col = "red", lwd=2)
  r2 <- round(cor(birds[[item-1]]$count, fitted(lreg2)), 4)
  mtext(r2, side=3)
}
dev.off()






