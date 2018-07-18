#' kulczynski
#'
#' This function calculates Kulczynski Index and the Kulczynski Distance. 
#' @param d results of function aggregatoR
#' @param method "s" Classical Jaccard similarity; "d" Classical Jaccard distance; 
#' @keywords aggregatoR
#' @details kulczynski is calculated according to Kulczynski Index
#' @references S. Kulczynski, "Zespoly rslin w pieninach", Bull. Int. Acad. Pol. Sci. Lettres, vol. 2, pp. 57-203, 1928.
#' @export
#' @seealso \code{\link{aggregatoR}}
#' @examples
#' data(macro_ex)
#' data.bio <- asBiomonitor(macro_ex)
#' data.agR <- aggregatoR(data.bio)

 

kulczynski <- function(y, method = "s"){
  # Remove taxa column
  y_no_taxa <- y1[,-1]
  # For each row and for each column calculate the relative abundance
  rel_abb <- matrix(nrow = nrow(y_no_taxa), ncol = ncol(y_no_taxa))
  for(i in 1:nrow(y_no_taxa)) {
    for(j in 1:ncol(y_no_taxa)) {
      rel_abb[i,j] <- y_no_taxa[i,j]/sum(y_no_taxa[ ,j])
    }
  }
  rel_abb <- as.data.frame(rel_abb)
  
  # Transform the abundance dataset in presence/absence dataset
  pa_df <- as.data.frame(ifelse(y_no_taxa >0, 1,0))
  # Creating all possible combinations between all columns with presence/absence data
  comb_pa <- combn(pa_df,2,simplify = F)
  # Creating all possible combinations between all columns with original abundance data
  comb_original <- combn(y_no_taxa,2,simplify = F)
  # Creating all possible combinations between all columns with relative abundance data
  comb_rel_ab <- combn(rel_abb,2,simplify = F)
  
  # Combination columns names
  t1 <- vector(length = length(comb_pa))
  for (j in 1:length(comb_pa)) {
    t <- paste(colnames(comb_pa[[j]][1]),"-",colnames(comb_pa[[j]][2]), sep="")
    t1[j] <- t
  }
  
  # Shared species between samples S12
  t2 <- vector(length = length(comb_pa))
  for(i in 1:length(comb_pa)){
    t <- sum(ifelse(comb_pa[[i]][,1] == comb_pa[[i]][,2],1,0))
    t2[i] <- t
  }
  S12 <- t2
  
  # S1 species > 0 in sample 1 
  t4 <- vector(length = length(comb_pa))
  for(i in 1:length(comb_pa)){
    t <- sum(ifelse(comb_pa[[i]][,1] > 0,1,0))
    t4[i] <- t
  }
  S1 <- t4
  
  # S2 species > 0 in sample 2
  t5 <- vector(length = length(comb_pa))
  for(i in 1:length(comb_pa)){
    t <- sum(ifelse(comb_pa[[i]][,2] > 0,1,0))
    t5[i] <- t
  }
  S2 <- t5
  
  # Kulczynski
  t3 <- vector(length = length(comb_pa))
  for (p in 1:length(comb_pa)) {
    t <- 0.5*((S12[p]/S1[p])+(S12[p]/S2[p]))
    t3[p] <- t 
  }
  
  # Kulczynski df [output]
  t3.df <- as.data.frame(t(t3))
  names(t3.df) <- t1
  
  if(method == "s"){
    output <- (t3.df)
  }
  
  if(method == "d"){
    output <- 1 - t3.df
  }
  return(output)
}