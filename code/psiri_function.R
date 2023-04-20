psiri <- function(x) {
  pn <- (x[[1]]$PercentNumber/x[[1]]$PercentOccurrence)/100 
  pw <- (x[[1]]$PercentVolWeight/x[[1]]$PercentOccurrence)/100 
  psi <- iri[[1]]$PercentOccurrence * (pn+pw)/2
  return(psi)
}