################################################################################
bioGBIF <- function(name_file = "taxa_GBIF", name_dir = "taxa_GBIF", download_directory = getwd(), download = F, 
                    kingdom.v = NA, phylum.v = NA, class.v = NA, order.v = NA, family.v = NA, genus.v = NA, accepted = T) {
  
  if (!require("data.table")) install.packages("data.table")
  require(data.table)
  
  if(download == T){
    if (!dir.exists(name_dir)){
      dir.create(name_dir)
    } else {
      stop("Directory already exists!")
    }
    print("Normally the download require a few minutes to terminate. Don't worry biomonitoR team offer you a coffee!")
  download.file("http://rs.gbif.org/datasets/backbone/backbone-current.zip", paste0(name_file,".zip"), method='auto')
  unzip(paste0(name_file,".zip"), exdir = paste0("./", name_dir))
  }
  
  taxa <- fread(paste0(download_directory,"/", name_dir, "/Taxon.tsv"), sep = "\t")
    
  if (accepted == T) {
    taxa <- subset(taxa, taxonomicStatus == "accepted")
  }
  
  if (!is.na(kingdom.v)) {
  taxa <- subset(taxa, kingdom == kingdom.v)
  } else if (!is.na(phylum.v)) {
  taxa <- subset(taxa, phylum == phylum.v)
  } else if (!is.na(class.v)) {
  taxa <- subset(taxa, class == class.v)
  } else if (!is.na(order.v)) {
  taxa <- subset(taxa, order == order.v)
  } else if (!is.na(family.v)) {
  taxa <- subset(taxa, family == family.v)
  } else if (!is.na(genus.v)) {
  taxa <- subset(taxa, genus == genus.v)
  }
  
    taxa.bior <- data.frame(Phylum=taxa$phylum, Class=taxa$class, Subclass=rep(NA, nrow(taxa)), Order=taxa$order,
               Family=taxa$family, Subfamily=rep(NA, nrow(taxa)), Tribus=rep(NA, nrow(taxa)), 
               Genus=taxa$genus, Species=taxa$specificEpithet, 
               Subspecies=taxa$infraspecificEpithet, Taxa=taxa$canonicalName)
      
    taxa.bior <- taxa.bior[!duplicated(taxa.bior["Taxa"]),]
  
  taxa.bior
}
################################################################################
setwd("/Users/tom/Desktop")
tt <- bioGBIF(genus.v = "Apodemus", download = F, accepted = T)


apo <- data.frame(Taxa = c("Apodemus flavicollis alpinus", "Apodemus rusiges", "Apodemus chevrieri", "Apodemus draco",
                           "Apodemus", "Apodemus agrarius", "Apodemus atavus", "Apodemus witherbyi"), 
                  Sample_1=c(6,15,32, 9,4,10, 20,43), Sample_2=c(3, 7,1,9,22,25,6,12))
apo.asb <- asBiomonitor(apo, dfref = tt, overwrite = T)

data.agR <- aggregatoR(apo.asb)
genNumb(data.agR)
famNumb(data.agR)

shannon(data.agR, taxLev = "Species")

abuTax(data.agR, taxa = c("draco"))