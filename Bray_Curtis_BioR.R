#' Bray-Curtis
#'
#' This function calculates Bray-Curtis Distance. 
#' @param d results of function aggregatoR
#' @param method "d" Bray-Curtis distance considering the abundance; 
#' @param method "dpa" Bray-Curtis distance considering presence/absence data;
#' @keywords aggregatoR
#' @details 
#' @references 
#' @export
#' @seealso \code{\link{aggregatoR}}
#' @examples
#' data(macro_ex)
#' data.bio <- asBiomonitor(macro_ex)
#' data.agR <- aggregatoR(data.bio)

brtis <- function(y, method = "d"){
  # Remove taxa column
  y_no_taxa <- y1[,-1]
  # Transform the abundance dataset in presence/absence dataset
  pa_df <- as.data.frame(ifelse(y_no_taxa >0, 1,0))
  # Creating all possible combinations between all columns with presence/absence data
  comb_pa <- combn(pa_df,2,simplify = F)
  # Creating all possible combinations between all columns with original abundance data
  comb_original <- combn(y_no_taxa,2,simplify = F)
  
  # Combination columns names
  t1 <- vector(length = length(comb_pa))
  for (j in 1:length(comb_pa)) {
    t <- paste(colnames(comb_pa[[j]][1]),"-",colnames(comb_pa[[j]][2]), sep="")
    t1[j] <- t
  }
  
  # B-C distance presence/absence
  t2 <- vector(length = length(comb_pa))
  for (j in 1:length(comb_pa)) {
    t <- sum(abs(comb_pa[[j]][,1] - comb_pa[[j]][,2]))
    t2[j] <- t
  }
  
  t2_bc <- vector(length = length(comb_pa))
  for (j in 1:length(comb_pa)) {
    t <- t2[j]/(sum(comb_pa[[j]][,1])+sum(comb_pa[[j]][,2]))
    t2_bc[j] <- t
  }
  
  t2.df <- as.data.frame(t(t2_bc))
  names(t2.df) <- t1
  
  # B-C distance abundance
  t3 <- vector(length = length(comb_original))
  for (j in 1:length(comb_original)) {
    t <- sum(abs(comb_original[[j]][,1] - comb_original[[j]][,2]))
    t3[j] <- t
  }
  
  t3_bc <- vector(length = length(comb_original))
  for (j in 1:length(comb_original)) {
    t <- t3[j]/(sum(comb_original[[j]][,1]) + sum(comb_original[[j]][,2]))
    t3_bc[j] <- t
  }
  
  t3.df <- as.data.frame(t(t3_bc))
  names(t3.df) <- t1
  
  if(method == "d"){
    output <- t3.df
  }
  
  if(method == "dpa"){
    output <- t2.df
  }
  return(output)
}