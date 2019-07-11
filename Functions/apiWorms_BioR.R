#' @name apiWorms
#' @title Function for creating reference databases using API from Worls Register of Marine Species (WORMS)
#'
#' @description This function allows creating our custon reference database using API from Worls Register of Marine Species (WORMS).
#' @param x Vector contain your Taxa list. This is the base list to create your custom reference database.
#' @param start_point First row number to start the creation of your reference database
#' @param end_point Last row number  to end the creation of your reference database
#' @param cknoFound logic. If = T it is possible check if are present taxa impossible to find with species names in WORMS. If there are some 'not found taxa' it is recommended use addWorms function.
#' @keywords darabase, reference, taxa
#' @seealso \code{\link{asBiomonitor}}
#' @seealso \code{\link{addWorms}}
#' @examples
#' refDB <- apiWorms(data$Taxa, end_point = nrow(data), cknoFound = T)
#' refDB$taxonomy
#' x.asb <- asBiomonitor(x, dfref = refDB$taxonomy, overwrite = T)
#' x.agR = aggregatoR(x.asb)


apiWorms <- function(x, start_point = 1, end_point = 1, cknoFound = F) {
  
  # Packages
  if (!require("rjson")) install.packages("rjson")
  require(rjson)
  if (!require("progress")) install.packages("progress")
  require(progress)
  if (!require("RecordLinkage")) install.packages("RecordLinkage")
  require(RecordLinkage)
  
  pb <- progress_bar$new(total = end_point)
  tax <- data.frame()
  statistic <- data.frame()
  notFound <- data.frame()
  notFound.pos <- data.frame()
  for(i in start_point:end_point) {
    taxon <- gsub(" ", "%20", x[i])
    ur.1 <- paste0("http://www.marinespecies.org/rest/AphiaRecordsByName/", taxon, "?like=true&marine_only=false&offset=1")
    content.1 <- fromJSON(file = ur.1)
    
    if(content.1[[1]]$status != "deleted") {
      taxa.bior <- data.frame(Phylum = ifelse(!is.null(content.1[[1]]$phylum), content.1[[1]]$phylum, NA),
                              Class = ifelse(!is.null(content.1[[1]]$class), content.1[[1]]$class, NA), 
                              Subclass = NA,
                              Order = ifelse(!is.null(content.1[[1]]$order), content.1[[1]]$order, NA),
                              Family = ifelse(!is.null(content.1[[1]]$family), content.1[[1]]$family, NA),
                              Subfamily = NA,
                              Tribus = NA,
                              Genus = ifelse(!is.null(content.1[[1]]$genus), content.1[[1]]$genus, NA),
                              Species = ifelse(!is.null(content.1[[1]]$valid_name), content.1[[1]]$valid_name, NA),
                              Subspecies = ifelse(content.1[[1]]$rank == "Subspecies", content.1[[1]]$scientificname, NA),
                              Taxa = ifelse(content.1[[1]]$status == "alternate representation", content.1[[1]]$scientificname, content.1[[1]]$valid_name))
      
      statistic.bior <- data.frame(original_taxa = x[i],
                                   valid_name = ifelse(!is.null(content.1[[1]]$valid_name), content.1[[1]]$valid_name, NA),
                                   rank = ifelse(!is.null(content.1[[1]]$rank), content.1[[1]]$rank, NA),
                                   status = ifelse(!is.null(content.1[[1]]$status), content.1[[1]]$status, NA),
                                   matchType = ifelse(!is.null(content.1[[1]]$match_type), content.1[[1]]$match_type, NA),
                                   similariry = round(levenshteinSim(as.character(x[i]), as.character(taxa.bior$Taxa)), digits=2))
      tax <- rbind(tax, taxa.bior)
      statistic <- rbind(statistic, statistic.bior)
      
    }  
    if(content.1[[1]]$status == "deleted") {
      notFound.bior <- data.frame(notFound = x[i],
                                  status = ifelse(!is.null(content.1[[1]]$status), content.1[[1]]$status, NA))
      
      for (j in 1:length(content.1)) {
        notFound.pos.bior <- data.frame(Taxa = x[i],
                                        status = ifelse(!is.null(content.1[[j]]$status), content.1[[j]]$status, NA),
                                        valid_AlphiaID = ifelse(!is.null(content.1[[j]]$valid_AphiaID), content.1[[j]]$valid_AphiaID, NA))
        
        notFound.pos <- rbind(notFound.pos, notFound.pos.bior) 
      }
      
      
      notFound <-rbind(notFound, notFound.bior)
    }
    
    pb$tick()
    Sys.sleep(start_point / end_point)
  }
  
  print(list(rank = table(statistic$rank),
             status = table(statistic$status),
             matchType = table(statistic$matchType),
             similariry = table(statistic$similariry),
             notFound = nrow(notFound)))
  
  if(cknoFound == F){
    list(taxonomy = tax, 
         taxStat = statistic,
         notFound = notFound)
  }
  
  if(cknoFound == T){
    list(taxonomy = tax, 
         taxStat = statistic,
         notFound = notFound,
         notFound.check = notFound.pos)
  }
  
}