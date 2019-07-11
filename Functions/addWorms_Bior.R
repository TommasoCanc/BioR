#' @name addWorms
#' @title Function for upgreating reference databases created by apiWorms.
#'
#' @description This function allows upgreating our custon reference database created previusly by apiWorms. This finction works using WORMS AlphiaID. 
#' @param starterDB Custom reference database obtained by apiWorms.
#' @param notFound.check Taxa not founded using apiWorms.
#' @keywords darabase, reference, taxa
#' @seealso \code{\link{asBiomonitor}}
#' @examples
#' refDB <- apiWorms(data$Taxa, end_point = nrow(data), cknoFound = T)
#' totDB <- addWorms(refDB, refDB$notFound.check)
#' totDB$taxonomy
#' x.asb <- asBiomonitor(x, dfref = totDB$taxonomy, overwrite = T)
#' x.agR = aggregatoR(x.asb)

addWorms <- function(starterDB, notFound.check){
  
  # Packages
  if (!require("rjson")) install.packages("rjson")
  require(rjson)
  if (!require("progress")) install.packages("progress")
  require(progress)
  if (!require("RecordLinkage")) install.packages("RecordLinkage")
  require(RecordLinkage)
  
  accepted.noFound <- notFound.check[notFound.check$status != "deleted", ]
  pb <- progress_bar$new(total = nrow(accepted.noFound))
  tax <- data.frame()
  statistic <- data.frame()
  for(i in 1:nrow(accepted.noFound)) {
    aphiaids <- accepted.noFound$valid_AlphiaID[i]
    ur.1 <- paste0("http://www.marinespecies.org/rest/AphiaRecordsByAphiaIDs?aphiaids%5B%5D=", aphiaids)
    content.1 <- fromJSON(file = ur.1)
    
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
                            Taxa = content.1[[1]]$valid_name)
    
    statistic.bior <- data.frame(original_taxa = accepted.noFound$Taxa[i],
                                 valid_name = ifelse(!is.null(content.1[[1]]$valid_name), content.1[[1]]$valid_name, NA),
                                 rank = ifelse(!is.null(content.1[[1]]$rank), content.1[[1]]$rank, NA),
                                 status = ifelse(!is.null(content.1[[1]]$status), content.1[[1]]$status, NA),
                                 matchType = ifelse(!is.null(content.1[[1]]$match_type), content.1[[1]]$match_type, NA),
                                 similariry = round(levenshteinSim(as.character(accepted.noFound$Taxa[i]), as.character(taxa.bior$Taxa)), digits=2))
    tax <- rbind(tax, taxa.bior)
    statistic <- rbind(statistic, statistic.bior)
    
    pb$tick()
    Sys.sleep(1 / nrow(accepted.noFound))
  }
  
  all.taxa <- rbind(starterDB$taxonomy, tax)
  all.statistic <- rbind(starterDB$taxStat, statistic)
  
  print(list(rank = table(all.statistic$rank),
             status = table(all.statistic$status),
             matchType = table(all.statistic$matchType),
             similariry = table(all.statistic$similariry)))
  
  list(taxonomy = all.taxa, 
       taxStat = all.statistic)
}