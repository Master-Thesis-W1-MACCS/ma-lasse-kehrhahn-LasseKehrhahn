################################################
# TRACKS THE PRODUCT LEVEL
################################################

.system_datalogging<-function(o,nn,EAD,DATA){
 # browser()
  
  

  DATApre = data.frame(o,nn,EAD$NUMB_C, EAD$NUMB_CN, EAD$NUMB_FR, EAD$NUMB_CM, EAD$NUMB_PV, EAD$NUMB_RC,
                       EAD$DENS_CCN, EAD$DENS_CNFR, EAD$DENS_FRCM, EAD$DENS_CMPV, EAD$DENS_PVRC,
                       EAD$Q_VAR, EAD$RCC_VAR,
                       EAD$Diff_total)
  
 colnames(DATApre) = c('o','nn','NUMB_C','NUMB_CN', "NUMB_FR", "NUMB_CM", "NUMB_PV", "NUMB_RC",
                     "DENS_CCN", "DENS_CNFR","DENS_FRCM","DENS_CMPV","DENS_PVRC",
                     "Q_VAR","RCC_VAR",
                     "DIFF_COST")    
 
  DATA = rbind(DATA,DATApre) #put it together
  
 
  
  
  
  return(DATA)
}


.product_datalogging<-function(o,nn,EAD,DATAp){
 
   browser()
  
  
  
  PRODUCT <- vector()
  DENS <- vector()
  RCC_VAR <- vector()
  CP <- vector()
  Error <-vector()
  NUMB_Error <-vector()
  CC <- vector()
  MISCPOOLSIZE <- vector()
  RUN <- vector()
  DESIGN <- vector ()
  PCb <- vector()
  PCh <- vector()
  Q <- vector()
  
  
  
  
  PRODUCT <- c(PRODUCT, 1:NUMB_PRO) #How many products per run 
  DENS[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DENS #Scaling firm parameter to products.
  Q_VAR[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR #Scaling firm parameter to products.
  RCC_VAR[PRODUCT] = FIRM$COSTING_SYSTEM$RC_VAR #Scaling firm parameter to products.
  CP[PRODUCT] = FIRM$COSTING_SYSTEM$CP #Scaling firm parameter to products.
  Error[PRODUCT] = FIRM$COSTING_SYSTEM$Error #Scaling firm parameter to products.
  NUMB_Error[PRODUCT] = FIRM$COSTING_SYSTEM$NUMB_Error #Scaling firm parameter to products.
  CC[PRODUCT] = FIRM$COSTING_SYSTEM$CC
  MISCPOOLSIZE[PRODUCT] = FIRM$COSTING_SYSTEM$MISCPOOLSIZE
  RUN[PRODUCT] = o #run, repetition
  DESIGN[PRODUCT] = nn #which kind of design? 
  
  
  PE = (FIRM$COSTING_SYSTEM$PCH - FIRM$COSTING_SYSTEM$PCB)/FIRM$COSTING_SYSTEM$PCB
  APE = abs((FIRM$COSTING_SYSTEM$PCH - FIRM$COSTING_SYSTEM$PCB))/FIRM$COSTING_SYSTEM$PCB
  PCb[PRODUCT] = FIRM$COSTING_SYSTEM$PCB
  PCh[PRODUCT] = FIRM$COSTING_SYSTEM$PCH
  Q[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DEMAND
  
  DATApre = data.frame(o,nn,PRODUCT,PCb,PCh,Q,PE,APE,DENS,Q_VAR,RCC_VAR,CP,Error,NUMB_Error,CC,MISCPOOLSIZE) # construct the dataframe 
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}
.input_datalogging <- function(FIRM,Input_DATA){
  
  
  Input_DATA = data.frame(FIRM$PRODUCTION_ENVIRONMENT$DENS,
                          FIRM$PRODUCTION_ENVIRONMENT$DENS_MIN,
                          FIRM$PRODUCTION_ENVIRONMENT$DENS_MAX,
                          FIRM$PRODUCTION_ENVIRONMENT$COR,
                          FIRM$PRODUCTION_ENVIRONMENT$Q_VAR,
                          FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                          FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,
                          FIRM$PRODUCTION_ENVIRONMENT$DISP1,
                          FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MIN,
                          FIRM$PRODUCTION_ENVIRONMENT$UNITLEVEL_ACT_SHARE_MAX,
                          FIRM$COSTING_SYSTEM$RC_VAR_MIN,
                          FIRM$COSTING_SYSTEM$RC_VAR_MAX,
                          FIRM$COSTING_SYSTEM$Error,
                          FIRM$COSTING_SYSTEM$NUMB_Error,
                          FIRM$COSTING_SYSTEM$CC,
                          FIRM$COSTING_SYSTEM$MISCPOOLSIZE,
                          FIRM$COSTING_SYSTEM$CP_HEURISTIC,
                          FIRM$COSTING_SYSTEM$CD_HEURISTIC)
    
   colnames(Input_DATA) = c('DENS','DENS_MIN','DENS_MAX','COR','Q_VAR','NUMB_PRO','NUMB_RES','DISP1','UNITLEVEL_ACT_SHARE_MIN',
                            'UNITLEVEL_ACT_SHARE_MAX','RC_VAR_MIN','RC_VAR_MAX','ERROR','NUMB_ERROR','CC','MISCPOOLSIZE','CP_HEURISTIC','CD_HEURISTIC')
   
   Input_DATA = t(Input_DATA)
   
   return(Input_DATA)
    
}



