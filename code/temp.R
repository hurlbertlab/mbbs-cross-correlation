
traits <- read.csv("data/traits/mbbsTraits.csv")
traits <- traits[, colSums(is.na(traits)) == 0]

emptyDataframe <- data.frame(Name = c(), uniqueValues = c(), range = c())

traitColumnNames <- names(traits)

for (i in 1:length(traitColumnNames)){
#for (i in 1:4){
  row <- traits[i]
  uniValues <- length(unique(row[1])[[1]])
  if (is.numeric(row[[1]])){
    range <- max(row) - min(row)
  } else{
    range <- NA
  }
  addRow <- data.frame(Name = c(traitColumnNames[i]), uniqueValues = c(uniValues), range = c(range))
  emptyDataframe <- rbind(emptyDataframe, addRow)
}
