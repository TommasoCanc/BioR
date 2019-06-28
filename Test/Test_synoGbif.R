# Library
library(biomonitoR)

# Function
source("./synoGbif_BioR.R")

# load example dataset
fish <- read.csv("./fish_prove_1.csv", sep=";")

# Replace and aggregate synonyms
rep.agr <- synoGbif(fish, end_point = nrow(fish))
rep.agr

