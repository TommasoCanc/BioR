# Library
library(biomonitoR)

# Function
source("/Users/tom/Desktop/GitHub/BioR/synoGbif_BioR.R")

# load example dataset
fish <- read.csv("/Users/tom/Desktop/GitHub/fish_prove_1.csv", sep=";")

# Replace and aggregate synonyms
rep.agr <- synoGbif(fish, end_point = nrow(fish))
rep.agr

