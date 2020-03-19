## BUILDING A REALIZED DEMAND VECTOR ##

####----current gen_Demand functions----####

.gen_Demand <- function(NUMB_C,TQ,Q_VAR){
  # This has been used in the Mertens (2020) for modeling dispersed realized demand
  if (Q_VAR == -1)
  {
    Q_VAR_MIN = 0.4
    Q_VAR_MAX = 1.6
    Q_VAR = runif(1, Q_VAR_MIN, Q_VAR_MAX)
  }
  
  preDemand = rlnorm(NUMB_C, meanlog = 1, sdlog = Q_VAR) #preDemand is buildup as a -> LogNormal Distribution 
  DEMAND = ceiling((preDemand/sum(preDemand))*TQ)
  EAD$Q_VAR_draw = Q_VAR
  
  #CHECKS 
  Qs = sort(DEMAND, decreasing = TRUE)
  EAD$CHECK$Q20 = sum(Qs[1:(0.2 * NUMB_C)])/TQ        #no. of units of 20% biggest products
  
  
  
  return(DEMAND)
}