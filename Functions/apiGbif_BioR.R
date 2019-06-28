apiGbif <- function(x, start_point = 1, end_point = 1) {
  
# Packages
if (!require("rgbif")) install.packages("rgbif")
require(rgbif)
if (!require("rjson")) install.packages("rjson")
require(rjson)
if (!require("progress")) install.packages("progress")
require(progress)
  
  pb <- progress_bar$new(total = end_point)
tax <- data.frame()
statistic <- data.frame()
for(i in start_point:end_point) {
  taxon <- gsub(" ", "%20", x[i])
  ur.1 <- paste0("http://api.gbif.org/v1/species/match?&name=", taxon)
  content.1 <- fromJSON(file = ur.1)
  
  taxa.bior <- data.frame(Phylum = ifelse(!is.null(content.1$phylum), content.1$phylum, NA), 
                          Class = ifelse(!is.null(content.1$class), content.1$class, NA), 
                          Subclass = NA,
                          Order = ifelse(!is.null(content.1$order), content.1$order, NA),
                          Family = ifelse(!is.null(content.1$family), content.1$family, NA),
                          Subfamily = NA,
                          Tribus = NA,
                          Genus = ifelse(!is.null(content.1$genus), content.1$genus, NA),
                          Species = ifelse(!is.null(content.1$species), content.1$species, NA),
                          Subspecies = ifelse(content.1$rank == "SUBSPECIES", content.1$scientificName, NA),
                          Taxa = content.1$canonicalName)
  
  statistic.bior <- data.frame(taxa = ifelse(!is.null(content.1$canonicalName), content.1$canonicalName, NA),
                          rank = ifelse(!is.null(content.1$rank), content.1$rank, NA),
                          status = ifelse(!is.null(content.1$status), content.1$status, NA),
                          matchType = ifelse(!is.null(content.1$matchType), content.1$matchType, NA),
                          confiance = ifelse(!is.null(content.1$confidence), content.1$confidence, NA))
  
  tax <- rbind(tax, taxa.bior)
  statistic <- rbind(statistic, statistic.bior)
  
  pb$tick()
  Sys.sleep(i / end_point)
}

print(list(rank = table(statistic$rank),
     status = table(statistic$status),
     matchType = table(statistic$matchType),
     confiance = table(statistic$confiance)))

list(taxonomy = tax, 
     taxStat = statistic)
}