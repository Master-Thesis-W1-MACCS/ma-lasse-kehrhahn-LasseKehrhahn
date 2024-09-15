#' Generate resource cost vektor
#'
#' Generates a random series of numbers, from which the sum is normalized to TC and returned
#' @param RC_VAR Standard deviation of the drawn numbers
#' @param TC Sum of the series of numbers
#' @param NUMB_RC Length of the series of numbers
#' @return Vektor which contains the randomly generated values
#' @example .gen_RCC(-1,1000,2)
#' @export

gen_RCC<- function(RC_VAR, TC, NUMB_RC)  
  
  {
  # INIT
  if (RC_VAR == -1)
  {
    RC_VAR_MIN = 0.4
    RC_VAR_MAX = 0.7
    RC_VAR = runif(1, RC_VAR_MIN, RC_VAR_MAX)
    RC_VAR = RC_VAR
  }
  
   preRCC = rlnorm(NUMB_RC, meanlog = 1, sdlog = RC_VAR)
   RCC = (preRCC/sum(preRCC))*TC #normalizing it #ceiled realized demand for each product
  
    ## Move the biggest resource to the front
  largest_RC <-
    sort(RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  RCC <- c(RCC[largest_RC], RCC[-largest_RC])
  
  ###CHECK###
  RCCs = sort(RCC, decreasing = TRUE)
  
  RCC20 = sum(RCCs[1:(0.2 * length(RCC))])/TC     #size of 20% biggest resources
  RCC10 = sum(RCCs[1:(0.1 * length(RCC))])/TC     #size of 10% biggest resources
  RCC02 = sum(RCCs[1:(0.02 * length(RCC))])/TC    #size of 2% biggest resources
  
  #plot(sort(RCC))
  
  #### sourcing

  return(RCC)
  
}

