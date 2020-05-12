#' Generates a matrix to define the relationship between CM and PV
#'
#' Random matrix with entries greater than 1 is generated somehow
#' @param EAD 
#' @param NUMB_CM 
#' @param NUMB_PV 
#' @param DENS_CMPV 
#' @return EAD ??
#' @example .gen_RES_CONS_PAT(EAD,2,2,-1)
#' @export

gen_RES_CONS_PAT <- function(EAD,NUMB_CM,NUMB_PV,DENS_CMPV){

## ====================== STEP 1 Determining the activities =========================
  UNITLEVEL_ACT_SHARE_MIN = 0.2    #0.2 is the size of DISP1 =10
  UNITLEVEL_ACT_SHARE_MAX = 0.4
  EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE = runif(1, UNITLEVEL_ACT_SHARE_MIN, UNITLEVEL_ACT_SHARE_MAX) #random activity share between lower and upper bounds
  
  EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE_MIN = UNITLEVEL_ACT_SHARE_MIN
  EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE_MAX = UNITLEVEL_ACT_SHARE_MAX
## ====================== STEP 1 Determining the amount of cost categories =================
  
  unitsize = floor(EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE*NUMB_PV)
  nonunitsize = NUMB_PV-unitsize
  
  EAD$PRODUCTION_TECHNOLOGY$UNITSIZE = unitsize
  EAD$PRODUCTION_TECHNOLOGY$NONUNITSIZE = nonunitsize

## ====================== STEP 0.b Determining the density (DENS)  =========================

  #Randomization and setting clear design points. 

  if(DENS_CMPV == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX)}
  else{DENS=DENS_CMPV}
    EAD$PRODUCTION_TECHNOLOGY$DENS_CMPV = DENS

## ====================== STEP 1 BASELINE NORM ========================= 

repeat    {
    
BASE = rnorm(NUMB_PV) #creates for every CO (product) a random number
  
RES_CONS_PATpre = matrix(rnorm(NUMB_CM*NUMB_PV,mean=0,sd=1), 
                         NUMB_CM, NUMB_PV)                            #random pre matrix, as Baseline

RES_CONS_PAT = matrix(0, nrow = NUMB_CM, ncol = NUMB_PV, byrow = TRUE) #empy matrix, that is going to be filled 

## ====================== STEP 1.a CORRELATION ========================= 
# Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
# Rows Products Colums Resources

COR1 =-1
COR2 =-1

# Correlation of  resources
if(COR1 == -1){
  COR1 <- runif(1, -0.2, 0.8)
}

sqrt_const_1 <- sqrt(1 - (COR1 * COR1))

# Correlation of the remaining resources
if(COR2 == -1){
  COR2 <- runif(1, -0.2, 0.8)
}

sqrt_const_2 <- sqrt(1 - (COR2 * COR2))

for (i in 1:(EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE*NUMB_PV)) #unitsize+1
{
  RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,i];
}

for (i in ((unitsize)+1) : NUMB_PV) #nonunitsize+1 (34+1)
{
  RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,i];
}

## ====================== STEP 1.b DENSITY ========================= 
res_cons_pat_b_pre = runif(NUMB_CM*NUMB_PV)

## 1/0 DENSITY
res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                          NUMB_CM,NUMB_PV)


RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PAT = RES_CONS_PAT

## ====================== STEP 1.c Ceiling and Scaling ============= 

# take absolute value of X and Z and scale by 10 and round them
# Anand et al. 2019
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
RES_CONS_PAT[,1] <- (BASE)
RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)

##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
RES_CONS_PAT_TOTAL <- RES_CONS_PAT * as.vector(EAD$CM)



##CALCULATING TCU
TCU <- colSums(RES_CONS_PAT_TOTAL)
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix

## ===================== EXCPETION HANDLER ====================

# EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019 # It is important the the first RES_CONS_PAT column has no zeros
# in accordance with Anand etl. 2019 and Balakrishnan et al. 2011; Substantiation of this hidden formalization remains unclear. 

PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource
RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?

if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
{
  break
}

}


## ====================== STEP 3 CHECK ========================= 

# AverageZeroConsumption
  EAD$PRODUCTION_TECHNOLOGY$NonZeroConsumption = sum(colSums(RES_CONS_PAT != 0))/     #Ratio of Zeros in Res_cons_pat
  (NUMB_CM * NUMB_PV)

# Average consumption of products consuming a resource
EAD$PRODUCTION_TECHNOLOGY$countNonZero<-mean(colSums(RES_CONS_PAT[,]>0))

# Correlation Test
# EAD$PRODUCTION_TECHNOLOGY$COR1<-mean(cor(RES_CONS_PAT[,1:(unitsize)])[1,])
  
# EAD$PRODUCTION_TECHNOLOGY$COR2<-mean(cor(RES_CONS_PAT[,c(1,((unitsize)+1):NUMB_PV)])[1,])

rownames(RES_CONS_PAT) = c(paste0('CM', 1:nrow(RES_CONS_PAT)))
colnames(RES_CONS_PAT) = c(paste0('PV', 1:ncol(RES_CONS_PAT)))


#Average distance 
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PAT = RES_CONS_PAT
EAD$A_CMPV = RES_CONS_PAT
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PATp = RES_CONS_PATp


#
#
#
return(EAD)


}


