#' euclidean
#'
#' This function calculates Euclidean Distance. 
#' @param d results of function aggregatoR
#' @param method "d" Euclidean distance considering the abundance; 
#' @param method "dpa" Euclidean distance considering presence/absence data;
#' @param method "av_d" Average Euclidean distance considering the abundance; "av_dpa" Average Euclidean distance considering presence/absence data;
#' @keywords aggregatoR
#' @details 
#' @references 
#' @export
#' @seealso \code{\link{aggregatoR}}
#' @examples
#' data(macro_ex)
#' data.bio <- asBiomonitor(macro_ex)
#' data.agR <- aggregatoR(data.bio)


euclidean <- function(y, method = "s"){
  # Remove taxa column
  y_no_taxa <- y1[,-1]
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
  
  # Euclidean distance presence/absence
  t2 <- vector(length = length(comb_pa))
  for (j in 1:length(comb_pa)) {
    t <- sqrt(sum((comb_pa[[j]][,1]-comb_pa[[j]][,2])^2))
    t2[j] <- t
  }
  
  t2.df <- as.data.frame(t(t2))
  names(t2.df) <- t1
  
  # Euclidean distance abundance
  t3 <- vector(length = length(comb_original))
  for (j in 1:length(comb_original)) {
    t <- sqrt(sum((comb_original[[j]][,1]-comb_original[[j]][,2])^2))
    t3[j] <- t
  }
  
  t3.df <- as.data.frame(t(t3))
  names(t3.df) <- t1
  
  # Average Euclidean Distance
  t4 <- sqrt((t3)^2/nrow(y_no_taxa))
  t4.df <- as.data.frame(t(t4))
  names(t4.df) <- t1
  t5 <- sqrt((t2)^2/nrow(y_no_taxa))
  t5.df <- as.data.frame(t(t5))
  names(t5.df) <- t1
  
  if(method == "d"){
    output <- t3.df
  }
  
  if(method == "dpa"){
    output <- t2.df
  }
  
  if(method == "av_d"){
    output <- t4.df
  }
  
  if(method == "av_dpa"){
    output <- t5.df
  }
  return(output)
}