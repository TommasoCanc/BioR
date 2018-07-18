#' sorensen
#'
#' This function calculates Sorensen Index and the Sorensen Distance. 
#' @param d results of function aggregatoR
#' @param method "s" Classical Sorensen similarity; "d" Classical Sorensen distance; 
#' @param method "ras" Relative Abbundance Sorensen Similarity; "rad" Relative Abbundance Sorensen Distance;
#' @param method "cras" Correction Relative Abbundance Sorensen Similarity; "crad" Correction Relative Abbundance Sorensen Distance;
#' @keywords aggregatoR
#' @details sorensen is calculated according to Sorensen Index
#' @references 
#' @export
#' @seealso \code{\link{aggregatoR}}
#' @examples
#' data(macro_ex)
#' data.bio <- asBiomonitor(macro_ex)
#' data.agR <- aggregatoR(data.bio)
#' bqies(data.agR)

sorensen <- function(y, method = "s"){
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
  
  # Sum same speies between pair of columns. a parameter in classical jaccard index
  t2 <- vector(length = length(comb_pa))
  for(i in 1:length(comb_pa)){
    t <- sum(ifelse(comb_pa[[i]][,1] == comb_pa[[i]][,2],1,0))
    t2[i] <- t
  }
  t2.df <- as.data.frame(t(t2))

  
  # Sorensen
  t3 <- vector(length = length(comb_pa))
  for (p in 1:length(comb_pa)) {
    t <- (2*t2.df[,p])/((2*t2.df[,p])+(nrow(y)-t2.df[,p]))
    t3[p] <- t 
  }
  
  # Sorensen df [output]
  t3.df <- as.data.frame(t(t3))
  names(t3.df) <- t1
  t3.df <- t3.df
  
  # Sorensen relative abundance
  colnames(rel_abb) <- colnames(y_no_taxa)
  
  # U: total relative abundances of the shared species in sample 1
  # V: total relative abundances of the shared species in sample 2
  U <- vector(length = length(comb_rel_ab))
  for (p in 1:length(comb_rel_ab)) {
    u <- comb_rel_ab[[p]][ ,1][comb_rel_ab[[p]][,1] > 0 & comb_rel_ab[[p]][,2] > 0]
    U[p] <- sum(u)
  }
  
  V <- vector(length = length(comb_rel_ab))
  for (p in 1:length(comb_rel_ab)) {
    v <- comb_rel_ab[[p]][ ,2][comb_rel_ab[[p]][,1] > 0 & comb_rel_ab[[p]][,2] > 0]
    V[p] <- sum(v)
  }
  
  UV <- U*V

  
  # UV/U+V-UV
  sor_ab_rel <- (2*UV)/(U+V)
  sor_ab_rel.df <- as.data.frame(t(sor_ab_rel))
  names(sor_ab_rel.df) <- t1
  
  # Sorensen relative abbundance [output]
  sor_ab_rel.df <- sor_ab_rel.df
  
  # Sorensen relative abundance with factor correction
  # Primo termine dell'equazione di Chao 2006 un Û è uguale a U per il campione 1
  U
  # Primo termine dell'equazione di Chao 2006 un V^ è uguale a V per il campione 2
  V
  
  #(m-1)/m
  m <- vector(length = length(comb_original))
  for (p in 1:length(comb_original)) {
    s_comb <- sum(comb_original[[p]][ ,2])
    m[p] <- s_comb
  }
  M_U <- (m-1)/m
  
  #(n-1)/n
  n <- vector(length = length(comb_original))
  for (p in 1:length(comb_original)) {
    s_comb <- sum(comb_original[[p]][ ,1])
    n[p] <- s_comb
  }
  N_V <- (n-1)/n
  
  f1U <- vector(length = length(comb_rel_ab))
  f2U <- vector(length = length(comb_rel_ab))
  for (p in 1:length(comb_rel_ab)) {
    f1U_1 <- sum(ifelse(comb_original[[p]][ ,1] == 1 & comb_original[[p]][ ,2] > 0, 1,0))
    f2U_1 <- sum(ifelse(comb_original[[p]][ ,1] == 2 & comb_original[[p]][ ,2] > 0, 1,0))
    f1U[p] <- f1U_1
    f2U[p] <- f2U_1
  }
  
  f1V <- vector(length = length(comb_rel_ab))
  f2V <- vector(length = length(comb_rel_ab))
  for (p in 1:length(comb_rel_ab)) {
    f1V_1 <- sum(ifelse(comb_original[[p]][ ,2] == 1 & comb_original[[p]][ ,1] > 0, 1,0))
    f2V_1 <- sum(ifelse(comb_original[[p]][ ,2] == 2 & comb_original[[p]][ ,1] > 0, 1,0))
    f1V[p] <- f1V_1
    f2V[p] <- f2V_1
  }
  
  # Relative abbundance considering only the species in the U
  # Then U*I
  UI <- vector(length = length(comb_rel_ab))
  for (p in 1:length(comb_rel_ab)) {
    UI1 <- sum(ifelse(comb_original[[p]][ ,2] == 1 & comb_original[[p]][ ,1] >= 1, comb_original[[p]][ ,1]/n[p],0))
    UI[p] <- UI1
  }
  
  # Relative abbundance considering only the species in the V
  # Then V*I
  VI <- vector(length = length(comb_rel_ab))
  for (p in 1:length(comb_rel_ab)) {
    VI1 <- sum(ifelse(comb_original[[p]][ ,1] == 1 & comb_original[[p]][ ,1] >= 1, comb_original[[p]][ ,2]/m[p],0))
    VI[p] <- VI1 
  }
  
  
  # Calcuation for U and V with correction
  U_correct <- vector(length = length(U))
  for (p in 1:length(U)) {
    U_cor <- U[p] + M_U[p] * (f1V[p]/(2*f2V[p])) * UI[p] 
    U_correct[p] <- U_cor 
  }
  
  V_correct <-vector(length = length(V))
  for (p in 1:length(V)) {
    V_cor <- V[p] + N_V[p] * (f1U[p]/(2*f2U[p])) * VI[p]
    V_correct[p] <- V_cor 
  }
  
  U_correct <- ifelse(U_correct > 1, 1, U_correct)
  V_correct <- ifelse(V_correct > 1, 1, V_correct)
  
  # Sorensen index with correction [output]
  sor_correct <- (2*(U_correct*V_correct))/(U_correct+V_correct)
  sor_correct.df <- as.data.frame(t(sor_correct))
  names(sor_correct.df) <- t1
  
  sor_correct <-vector(length = length(U))
  for (p in 1:length(V)) {
    s_cor <- (2*(U_correct[p]*V_correct[p]))/(U_correct[p]+V_correct[p])
    sor_correct[p] <- s_cor
  }
  sor_correct.df <- as.data.frame(t(sor_correct))
  names(sor_correct.df) <- t1
  
  
  if(method == "s"){
    output <- (t3.df)
  }
  
  if(method == "d"){
    output <- 1 - t3.df
  }
  
  if(method == "ras"){
    output <- (sor_ab_rel.df)
  }
  
  if(method == "rad"){
    output <- 1 - sor_ab_rel.df
  }
  
  if(method == "cras"){
    output <- (sor_correct.df)
  }
  
  if(method == "crad"){
    output <- 1 - sor_correct.df
  }
  return(output)
}