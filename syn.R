################################################################################
syn <- function(dataset = taxa.bior.1, reference_DB_GBIF = taxa, accepted = F, doubtful = F, synonym = F,
                homotypic.synonym = F, misapplied = F, proparte.synonym = F, heterotypic.synonym = F) {
  
  colnames(reference_DB_GBIF)[colnames(reference_DB_GBIF)=="canonicalName"] <- "Taxa"
  taxa.merge <- merge(x = dataset, y = reference_DB_GBIF, by = "Taxa", all.x =T)
  taxa.merge$Taxa = as.character(taxa.merge$Taxa)
  tab.taxonomicStatus <- data.frame(table(taxa.merge$taxonomicStatus))
  colnames(tab.taxonomicStatus) <- c("TaxonomicStatus", "Freq")
  print(tab.taxonomicStatus)
  
  if (accepted == T) {
    accepted <- list(AcceptedName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "accepted"]))
    print(accepted)
  }
  
  if (doubtful == T) {
    print(list(DoubtfulName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "doubtful"])))
  }
  
  if (synonym == T) {
    print(list(SynonymName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "synonym"])))
  }
  
  if (homotypic.synonym == T) {
    print(list(HomotypicSynonymName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "homotypic synonym"])))
  }
  
  if (misapplied == T) {
    print(list(MisappliedName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "misapplied"])))
  }
  
  if (proparte.synonym == T) {
    print(list(Proparte.SynonymName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "proparte synonym"])))
  }
  
  if (heterotypic.synonym == T) {
    print(list(Heterotypic.SynonymName=(taxa.merge$Taxa[taxa.merge$taxonomicStatus == "heterotypic synonym"])))
  }
  
  syn <- data.frame(acceptedID = taxa.merge$acceptedNameUsageID, SynonymsName = taxa.merge$Taxa)
  syn <- syn[!is.na(syn$acceptedID),]
  che <- data.frame(acceptedID = reference_DB_GBIF$taxonID, AcceptedName = reference_DB_GBIF$Taxa)
  out <- merge(x = syn, y = che, by="acceptedID", all.x = T)
  
  rm(syn)
  rm(che)
  return(out[,2:3])
}  

################################################################################
aa <- syn(taxa.bior.1,taxa, accepted = T, doubtful = T, homotypic.synonym = T, synonym = T)
unique(taxa.1$taxonomicStatus)