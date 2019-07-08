# Possible statuses for a name:
#   
# Accepted: the used name is accepted in the present literature
# 
# Unaccepted: The used name is NOT accepted in the present literature
# 
# Nomen nudum: a name that does not comply with the name requirements of the codes, 
# such as lack of a description or diagnoses or reference to a description or diagnosis 
# or a type specimen is lacking for publications after 1999
# 
# Alternate representation: to link species that are represented twice: once with 
# and once without subgenus. Alternate representation can also be used for a 
# species and its nominal subspecies (note: you can only add a subspecies if the 
# species is present in the database).
# 
# Nomen dubium: a name of uncertain application, because it is not possible to 
# establish the taxon to which it should be referred. A good example is the 
# "Ascothoracida" genus Laocoon. There is a debate whether this is based on a 
# parasite or on a detached piece of the host. It is clearly a dubious name
# 
# Temporary name: to create higher rank taxa to accomodate child taxa for which 
# the classification is not sorted yet
# 
# Taxon inquirendum: an incompletely defined taxon that requires further 
# characterization, it is impossible to identify the taxon
# 
# Interim unpublished: an as yet unavailable name (until in a print issue) which 
# has been published online only, in a work that does not show evidence of ZooBank 
# registration (ICZN Article 8.5)


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

