#' @name apiGbif
#' @title Function for creating reference databases using API from Global Biodiversity Information Facility  (GBIF)
#'
#' @description This function allows creating our custon reference database using API from Global Biodiversity Information Facility (GBIF).
#' @param x Vector contain your Taxa list. This is the base list to create your custom reference database.
#' @param start_point First row number to start the creation of your reference database.
#' @param end_point Last row number to end the creation of your reference database.
#' @param cksyno If T all possible synonyms are substituted with their accepted name in the reference database Taxa column
#' @keywords darabase, reference, taxa
#' @seealso \code{\link{asBiomonitor}}
#' @seealso \code{\link{synoGbif}}
#' @examples
#' refDB <- apiGbif(data$Taxa, end_point = nrow(data), cksyno = F)
#' refDB.bior <- refDB$taxonomy
#' x.asb <- asBiomonitor(x, dfref = refDB.bior, overwrite = T)
#' x.agR = aggregatoR(x.asb)


apiGbif <- function(x, start_point = 1, end_point = 1, cksyno = F) {
  
  # Packages
  if (!require("rgbif")) install.packages("rgbif")
  require(rgbif)
  if (!require("rjson")) install.packages("rjson")
  require(rjson)
  if (!require("progress")) install.packages("progress")
  require(progress)
  
  if(cksyno == F) {
    pb <- progress_bar$new(total = end_point)
    tax <- data.frame()
    statistic <- data.frame()
    similarity <- data.frame()
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
                              Species = ifelse(!is.null(content.1$species), content.1$species, content.1$canonicalName),
                              Subspecies = ifelse(content.1$rank == "SUBSPECIES", content.1$scientificName, NA),
                              Taxa = content.1$canonicalName)
      
      statistic.bior <- data.frame(original_taxa = ifelse(!is.null(content.1$canonicalName), content.1$canonicalName, NA),
                                   accepted_name = ifelse(!is.null(content.1$species), content.1$species, content.1$canonicalName),
                                   rank = ifelse(!is.null(content.1$rank), content.1$rank, NA),
                                   status = ifelse(!is.null(content.1$status), content.1$status, NA),
                                   confidence = ifelse(!is.null(content.1$confidence), content.1$confidence, NA))
      
      
      tax <- rbind(tax, taxa.bior)
      statistic <- rbind(statistic, statistic.bior)
      
      pb$tick()
      Sys.sleep(i / end_point)
    }
  } 
  
  # Se = T nel database finale i sinonime vengono anche raggrupati, in modo tale da non avere 
  # doppioni. Controllare bene!!!!!!!!
  if(cksyno == T) {
    pb <- progress_bar$new(total = end_point)
    tax <- data.frame()
    statistic <- data.frame()
    for(i in start_point:end_point) {
      taxon <- gsub(" ", "%20", x[i])
      ur.1 <- paste0("http://api.gbif.org/v1/species/match?&name=", taxon)
      content.1 <- fromJSON(file = ur.1)
      
      if(content.1$status != "SYNONYM") {
        taxa.bior <- data.frame(Phylum = ifelse(!is.null(content.1$phylum), content.1$phylum, NA), 
                                Class = ifelse(!is.null(content.1$class), content.1$class, NA), 
                                Subclass = NA,
                                Order = ifelse(!is.null(content.1$order), content.1$order, NA),
                                Family = ifelse(!is.null(content.1$family), content.1$family, NA),
                                Subfamily = NA,
                                Tribus = NA,
                                Genus = ifelse(!is.null(content.1$genus), content.1$genus, NA),
                                Species = ifelse(!is.null(content.1$species), content.1$species, content.1$canonicalName),
                                Subspecies = ifelse(content.1$rank == "SUBSPECIES", content.1$scientificName, NA),
                                Taxa = content.1$canonicalName)
        
        statistic.bior <- data.frame(original_taxa = ifelse(!is.null(content.1$canonicalName), content.1$canonicalName, NA),
                                     accepted_name = ifelse(!is.null(content.1$species), content.1$species, content.1$canonicalName),
                                     rank = ifelse(!is.null(content.1$rank), content.1$rank, NA),
                                     status = ifelse(!is.null(content.1$status), content.1$status, NA),
                                     confidence = ifelse(!is.null(content.1$confidence), content.1$confidence, NA))
        
        tax <- rbind(tax, taxa.bior)
        statistic <- rbind(statistic, statistic.bior)
      }
      
      if(content.1$status == "SYNONYM") {
        usageKey <- content.1$speciesKey
        ur.2 <- paste0("http://api.gbif.org/v1/species/", usageKey)
        content.2 <- fromJSON(file = ur.2)
        
        
        taxa.bior <- data.frame(Phylum = ifelse(!is.null(content.2$phylum), content.2$phylum, NA), 
                                Class = ifelse(!is.null(content.2$class), content.2$class, NA), 
                                Subclass = NA,
                                Order = ifelse(!is.null(content.2$order), content.2$order, NA),
                                Family = ifelse(!is.null(content.2$family), content.2$family, NA),
                                Subfamily = NA,
                                Tribus = NA,
                                Genus = ifelse(!is.null(content.2$genus), content.2$genus, NA),
                                Species = ifelse(!is.null(content.2$species), content.2$species, content.2$species),
                                Subspecies = ifelse(content.2$rank == "SUBSPECIES", content.2$scientificName, NA),
                                Taxa = content.2$species)
        
        statistic.bior <- data.frame(original_taxa = x[i],
                                     accepted_name = ifelse(!is.null(content.2$species), content.2$species, content.2$canonicalName),
                                     rank = ifelse(!is.null(content.2$rank), content.2$rank, NA),
                                     status = ifelse(!is.null(content.2$taxonomicStatus), content.2$taxonomicStatus, NA),
                                     confidence = ifelse(!is.null(content.1$confidence), content.1$confidence, NA))
        
        tax <- rbind(tax, taxa.bior)
        statistic <- rbind(statistic, statistic.bior)
        
      }
      
      pb$tick()
      Sys.sleep(i / end_point)
    }
    tax <- tax[!duplicated(tax$Taxa),]
    rownames(tax) <- seq(1, nrow(tax))
  } 
  
  print(list(rank = table(statistic$rank),
             status = table(statistic$status),
             confidence = table(statistic$confidence)))
  
  list(taxonomy = tax, 
       taxStat = statistic)
}