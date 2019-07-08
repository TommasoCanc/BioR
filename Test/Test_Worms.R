# Library
library(biomonitoR)

# Functions
source("/Users/tom/Desktop/GitHub/BioR/apiWorms_BioR.R")
source("/Users/tom/Desktop/GitHub/BioR/addWorms_Bior.R")

# load example dataset
fish <- read.csv("/Users/tom/Desktop/GitHub/BioR_GitHub/BioR/Datasets/fish_prove_1.csv", sep=";")

# Create reference database from WORMS API
refDB <- apiWorms(fish$Taxa, end_point = nrow(fish), cknoFound = T)
refDB$taxonomy
refDB$taxStat
refDB$notFound
refDB$notFound.check

# If you would add the taxa not founded with the function apiWorms(), you could 
# use addWorms() function

totDB <- addWorms(refDB, refDB$notFound.check)
totDB$taxonomy

fish.asb <- asBiomonitor(fish, dfref = totDB$taxonomy, overwrite = T)
fish.agR = aggregatoR(fish.asb)

richness(fish.agR, "Family")
richness(fish.agR, "Genus")

