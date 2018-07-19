#' bqies
#'
#' This function calculates the Benthic Quality Index for Italian Lakes. 
#' Boggero et al. (2016), Italian classification method for macroinvertebrates in lakes. Method summary.
#' @param d results of function aggregatoR
#' @keywords aggregatoR
#' @details bqies is calculated according to Benthic Quality Index for Italian Lakes
#' @references Boggero A., Zaupa S., Cancellario T., Lencioni V., Marziali L., Rossaro B., (2016), Italian classification method for macroinvertebrates in lakes. Method summary. Technical Report
#' @export
#' @seealso \code{\link{aggregatoR}}
#' @examples
#' data(macro_bqies_ex)
#' data.bio <- asBiomonitor(macro_bqies_ex)
#' data.agR <- aggregatoR(data.bio)
#' bqies(data.agR)
 
bqies <- function(x,y, overwrite=F){
  # Facciamo un merge tra le tabelle pesi e dati, in quetso modo creiamo una tabella contenente
  # solo le specie che hanno un peso
  d_merg <- merge(x, y)
  # Rimuoviamo la colonna con i pesi dalla tabbella d-merge
  d_merg.noWeight <- subset(d_merg, select = -c(Scores))
  # Calcoliamo il Log10+1 di tutte le specie con un peso
  d_merg.log <- log10(d_merg.noWeight[, -1]+1)
  # Sommiamo di tutti i log per colonna
  d_merg.sum <- colSums(d_merg.log)
  # Contiamo il numero di valori diverso da 0 in d_merg
  d_merg.no0 <- colSums(d_merg.log > 0)
  # Calcoliamo il numero di specie non presenti in elenco
  spec_no_presence <- colSums(x[,-1] > 0) - d_merg.no0
  # Calcoliamo il BQI
  bqi <- (colSums(d_merg.log*d_merg$Scores))/d_merg.sum
  bqi <- round(bqi, digits = 3)
  # Calcoliamo il BQIES
  bqies <- bqi*log10(d_merg.no0 +  spec_no_presence +1)* colSums(x[,-1])/(colSums(x[,-1])+5)
  bqies <- round(bqies, digits = 3)
  # Calcoliamo la percentuale per la densità dei taxa con peso indicatore
  percent <- (1-(colSums(x[,-1])-colSums(d_merg.noWeight[,-1]))/colSums(x[,-1]))*100
  percent <- round(percent, digits = 1)
  
  output<-list("BQI" = bqi,"BQIES" = bqies, "N. Taxa without Score" = spec_no_presence, 
               "% Taxa with Score"=percent)
  
  return(output)
  
}