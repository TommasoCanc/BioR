# Library
library(biomonitoR)

# Function
source("./apiFbase_BioR.R")

# load example dataset
fish <- read.csv("./fish_prove.csv", sep=";")

# Create reference database from GBIF API
refDB <- apiFbase(fish$Taxa, end_point = nrow(fish))
refDB.bior <- refDB$taxonomy

fish.asb <- asBiomonitor(fish, dfref = refDB.bior, overwrite = T)
fish.agR = aggregatoR(fish.asb)

levenshteinSim("Oncorhynchus masou masou", "Oncorhynchus_gorbuscha")
levenshteinSim("Oncorhynchus masou masou", "Oncorhynchus masou")

richness(fish.agR, "Family")
richness(fish.agR, "Genus")