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
 
  
  ####### DOES NOT WORK YET ############
  
  
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

