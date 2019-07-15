# Library
library(biomonitoR)

# Function
source("./apiGbif_BioR.R")

# load example dataset
fish <- read.csv("./fish_prove_1.csv", sep=";")

# Create reference database from GBIF API
refDB <- apiGbif(fish$Taxa, end_point = nrow(fish), cksyno = F)
refDB$taxonomy
refDB$taxStat

# If cksyno = T all synonyms are substituted by accepted name and all possible duplicated are 
# aggregated.
refDB.1 <- apiGbif(fish$Taxa, end_point = nrow(fish), cksyno = T)
refDB.1$taxonomy
refDB.1$taxStat


fish.asb <- asBiomonitor(fish, dfref = refDB.bior, overwrite = T)
fish.agR = aggregatoR(fish.asb)

richness(fish.agR, "Family")
richness(fish.agR, "Genus")

