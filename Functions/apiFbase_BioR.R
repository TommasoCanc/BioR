apiFbase <- function(x, start_point = 1, end_point = 1) {

# Packages
if (!require("progress")) install.packages("progress")
require(progress)
if (!require("stringr")) install.packages("stringr")
require(stringr)
if (!require("RecordLinkage")) install.packages("RecordLinkage")
require(RecordLinkage)

pb <- progress_bar$new(total = end_point)
tax <- data.frame()
statistic <- data.frame()
for(i in start_point:end_point) {
  taxon.1 <- word(x[i],1)
  taxon.2 <- word(x[i],-1)
  
  ur.1 <- paste0("https://fishbase.ropensci.org/taxa?Genus=", taxon.1, "&Species=", taxon.2)
  content.1 <- fromJSON(file = ur.1)

  
  taxa.bior <- data.frame(Phylum = "Chordata",
                          Class = ifelse(!is.null(content.1$data[[1]]$Class), content.1$data[[1]]$Class, NA), 
                          Subclass = NA,
                          Order = ifelse(!is.null(content.1$data[[1]]$Order), content.1$data[[1]]$Order, NA),
                          Family = ifelse(!is.null(content.1$data[[1]]$Family), content.1$data[[1]]$Family, NA),
                          Subfamily = ifelse(!is.null(content.1$data[[1]]$SubFamily), content.1$data[[1]]$SubFamily, NA),
                          Tribus = NA,
                          Genus = ifelse(!is.null(content.1$data[[1]]$Genus), content.1$data[[1]]$Genus, NA),
                          Species = ifelse(!is.null(content.1$data[[1]]$Species), paste(content.1$data[[1]]$Genus, content.1$data[[1]]$Species, sep = " "), NA),
                          Taxa = ifelse(!is.null(content.1$data[[1]]$Species), paste(content.1$data[[1]]$Genus, content.1$data[[1]]$Species, sep = " "), NA))
  
  statistic.bior <- data.frame(original_taxa = x[i],
                               valid_name = ifelse(!is.null(content.1$data[[1]]$Species), paste(content.1$data[[1]]$Genus, content.1$data[[1]]$Species, sep = " "), NA),
                               similariry = round(levenshteinSim(as.character(x[i]), as.character(taxa.bior$Taxa)), digits=2))
  
  tax <- rbind(tax, taxa.bior)
  statistic <- rbind(statistic, statistic.bior)
  
  pb$tick()
  Sys.sleep(start_point / end_point)
  
}

print(list(similarity = table(statistic$similariry)))

list(taxonomy = tax, 
     taxStat = statistic)
}