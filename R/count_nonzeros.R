count_nonzeros <-
function(your.matrix){
  colsum_nonzeros = colSums(your.matrix != 0)
  percentageofnonzeros = sum(colsum_nonzeros)/(ncol(your.matrix)*nrow(your.matrix))
  return(percentageofnonzeros)
}
