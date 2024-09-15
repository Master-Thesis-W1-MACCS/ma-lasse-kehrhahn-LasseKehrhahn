#' Generate Demand Vektor
#'
#' Generates a random series of numbers, from which the sum is normalized to TQ and returned
#' @param NUMB_C Length of the series of numbers
#' @param TQ Sum of the series of numbers
#' @param Q_VAR Standard deviation of the drawn numbers
#' @return Vektor which contains the randomly generated values
#' @example .gen_Demand(1,100,-1)
#' @export

gen_Demand <- function(NUMB_C,TQ,Q_VAR){
  # This has been used in the Mertens (2020) for modeling dispersed realized demand

  if (Q_VAR == -1)
  {
    Q_VAR_MIN = 0.4
    Q_VAR_MAX = 1.6
    Q_VAR = runif(1, Q_VAR_MIN, Q_VAR_MAX) #runif = Uniform Distribution on interval min to max.
  }

  preDemand = rlnorm(NUMB_C, meanlog = 1, sdlog = Q_VAR) #preDemand is buildup as a -> Log Normal Distribution whose logarithm has mean equal to meanlog and standard deviation equal to sdlog
  DEMAND = ceiling((preDemand/sum(preDemand))*TQ) #ceiling = runden
  EAD$Q_VAR_draw = Q_VAR

  #CHECKS
  Qs = sort(DEMAND, decreasing = TRUE)
  EAD$CHECK$Q20 = sum(Qs[1:(0.2 * NUMB_C)])/TQ        #no. of units of 20% biggest products

  return(DEMAND)
}
