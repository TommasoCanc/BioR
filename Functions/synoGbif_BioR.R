synoGbif <- function(x, start_point = 1, end_point = 1) {
  
  # Packages
  if (!require("rgbif")) install.packages("rgbif")
  require(rgbif)
  if (!require("rjson")) install.packages("rjson")
  require(rjson)
  if (!require("progress")) install.packages("progress")
  require(progress)
  if (!require("plyr")) install.packages("plyr")
  require(plyr)
  
  pb <- progress_bar$new(total = end_point)
  statistic <- data.frame()
  for(i in start_point:end_point) {
    taxon <- gsub(" ", "%20", x$Taxa[i])
    ur.1 <- paste0("http://api.gbif.org/v1/species/match?&name=", taxon)
    content.1 <- fromJSON(file = ur.1)
    
    statistic.bior <- data.frame(original_taxa = content.1$canonicalName,
                                 accepted_name = ifelse(!is.null(content.1$species), content.1$species, content.1$canonicalName),
                                 status = ifelse(!is.null(content.1$status), content.1$status, NA))
    
    statistic <- rbind(statistic, statistic.bior)
    
    pb$tick()
    Sys.sleep(i / end_point)
  }
  
  # Aggregate duplicate
  dup.agr <- cbind(statistic$accepted_name, x)  
  dup.agr <- ddply(dup.agr,"statistic$accepted_name",numcolwise(sum))
  colnames(dup.agr)[1] <- "Taxa"
  
  remplace.synonyms <- statistic[statistic$status == "SYNONYM",]
  
  list(dataset = dup.agr, 
       remplace = remplace.synonyms)
  
}
