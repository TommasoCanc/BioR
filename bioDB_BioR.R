################################################################################
bioDB <- function(x, overwrite=F){
  
  colnames(x) <- c("taxonid","datasetid","id_synonym","taxonomicstatus",
                   "taxonrank","scientificname","kingdom",
                   "phylum","class","subclass","order","superfamily",
                   "family", "subfamily","tribe","genericname","genus",
                   "subgenus","specificepithet","infraspecificepithet",
                   "scientificnameauthorship","modified","reference") 
  
  x_filt <- x[ ,c(5,8:11,13,14:16,17,19,20)]
  
  x_filt$taxonrank[which(x_filt$taxonrank == "")] <- NA
  x_filt$phylum[which(x_filt$phylum == "")] <- NA
  x_filt$class[which(x_filt$class == "")] <- NA
  x_filt$subclass[which(x_filt$subclass == "")] <- NA
  x_filt$order[which(x_filt$order == "")] <- NA
  x_filt$family[which(x_filt$family == "")] <- NA
  x_filt$subfamily[which(x_filt$subfamily == "")] <- NA
  x_filt$tribe[which(x_filt$tribe == "")] <- NA
  x_filt$genericname[which(x_filt$genericname == "")] <- NA
  x_filt$genus[which(x_filt$genus == "")] <- NA
  x_filt$specificepithet[which(x_filt$specificepithet == "")] <- NA
  x_filt$infraspecificepithet[which(x_filt$infraspecificepithet == "")] <- NA
  x_filt$species <- ifelse(!is.na(x_filt$infraspecificepithet), paste(x_filt$genericname, x_filt$specificepithet,
                                                                      x_filt$infraspecificepithet, sep=" "), paste(x_filt$genericname, x_filt$specificepithet, sep=" "))
  x_filt$species<- gsub("NA", "", x_filt$species)
  
  x_filt$Taxa.group <- ifelse(x_filt$taxonrank == "group",paste(x_filt$species), NA)
  x_filt$Taxa.phylum <- ifelse(x_filt$taxonrank == "phylum",paste(x_filt$phylum), NA)
  x_filt$Taxa.class <- ifelse(x_filt$taxonrank == "class",paste(x_filt$class), NA)
  x_filt$Taxa.subclass <- ifelse(x_filt$taxonrank == "subclass",paste(x_filt$subclass), NA)
  x_filt$Taxa.order <- ifelse(x_filt$taxonrank == "order",paste(x_filt$order), NA)
  x_filt$Taxa.family <- ifelse(x_filt$taxonrank == "family",paste(x_filt$family), NA)
  x_filt$Taxa.tribe <- ifelse(x_filt$taxonrank == "tribe",paste(x_filt$tribe), NA)
  x_filt$Taxa.genus <- ifelse(x_filt$taxonrank == "genus",paste(x_filt$genus), NA)
  x_filt$Taxa.species <- ifelse(x_filt$taxonrank == "species",paste(x_filt$species), NA)
  x_filt$Taxa.infraspecies <- ifelse(x_filt$taxonrank == "infraspecies",paste(x_filt$species), NA)
  
  x_filt$taxa = x_filt$Taxa.phylum
  x_filt$taxa[!is.na(x_filt$Taxa.group)] = x_filt$Taxa.group[!is.na(x_filt$Taxa.group)]
  x_filt$taxa[!is.na(x_filt$Taxa.class)] = x_filt$Taxa.class[!is.na(x_filt$Taxa.class)]
  x_filt$taxa[!is.na(x_filt$Taxa.subclass)] = x_filt$Taxa.subclass[!is.na(x_filt$Taxa.subclass)]
  x_filt$taxa[!is.na(x_filt$Taxa.order)] = x_filt$Taxa.order[!is.na(x_filt$Taxa.order)]
  x_filt$taxa[!is.na(x_filt$Taxa.family)] = x_filt$Taxa.family[!is.na(x_filt$Taxa.family)]
  x_filt$taxa[!is.na(x_filt$Taxa.tribe)] = x_filt$Taxa.tribe[!is.na(x_filt$Taxa.tribe)]
  x_filt$taxa[!is.na(x_filt$Taxa.genus)] = x_filt$Taxa.genus[!is.na(x_filt$Taxa.genus)] 
  x_filt$taxa[!is.na(x_filt$Taxa.species)] = x_filt$Taxa.species[!is.na(x_filt$Taxa.species)]
  x_filt$taxa[!is.na(x_filt$Taxa.infraspecies)] = x_filt$Taxa.infraspecies[!is.na(x_filt$Taxa.infraspecies)]
  
  
  x_bior <- x_filt[ ,c(2:8,10,22,23,24)]
  colnames(x_bior)[c(1:11)] <- c("Phylum", "Class", "Subclass", "Order", "Family", "Subfamily",
                                 "Tribus", "Genus", "Species", "Subspecies","Taxa")
  
  x_bior <- x_bior[complete.cases(x_bior$Taxa),]
  
  x_bior$Phylum[is.na(x_bior$Phylum)] <- ""
  x_bior$Class[is.na(x_bior$Class)] <- ""
  x_bior$Subclass[is.na(x_bior$Subclass)] <- ""
  x_bior$Order[is.na(x_bior$Order)] <- ""
  x_bior$Family[is.na(x_bior$Family)] <- ""
  x_bior$Subfamily[is.na(x_bior$Subfamily)] <- ""
  x_bior$Tribus[is.na(x_bior$Tribus)] <- ""
  x_bior$Genus[is.na(x_bior$Genus)] <- ""
  x_bior$Species[is.na(x_bior$Species)] <- ""
  x_bior$Subspecies[is.na(x_bior$Subspecies)] <- ""
  x_bior$Taxa[is.na(x_bior$Taxa)] <- ""
  
  x_bior
}
################################################################################