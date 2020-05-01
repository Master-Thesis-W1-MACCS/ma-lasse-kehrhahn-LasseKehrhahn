gen_EAD <-
function(EAD,TQ) {

  #  % 23.4. - Generates the customers in the market Customer generation function
  #  % 24.7. - Make customers' attributes more precise, in particular CN and FRN
  #  % 01.10 - Code simplification 
  #  % 04.2  - Cost calculation implemented
  #  % 05.02 - Simulation has been implemented 
  #  % 25.02 - Update Product architecture generation
  #  % 19.03 - Update Demand function
  #  % 19.03 - Update Customer diversity 
  #  % 19.03 - Market structure
  
  
  
  
  #### PREDETERMINING ====   #aus 01 INIT?
  DENS_CCN =  EAD$DENS_CCN
  DENS_CNFR = EAD$DENS_CNFR
  DENS_FRCM = EAD$DENS_FRCM
  DENS_CMPV = EAD$DENS_CMPV
  DENS_PVRC = EAD$DENS_PVRC 
  NUMB_C = EAD$NUMB_C #Customers
  NUMB_CN = EAD$NUMB_CN #Customer' needs 
  NUMB_FR = EAD$NUMB_FR #Functional Requirements
  NUMB_CM = EAD$NUMB_CM #Components / Designparameter
  NUMB_PV = EAD$NUMB_PV #Processes
  NUMB_RC = EAD$NUMB_RC #Resources
  Q_VAR = EAD$Q_VAR
  
  #### DEMAND GENERATION ####      # <- ist quasi das Selbe wie =   # %*% ist Matrix mult. Operator
  #aus designfunctions: .create_designmatrix <- function(X,Y,DENS,rowname="X",colname="Y")
  
  C_DEMAND <- .gen_Demand(NUMB_C, TQ, Q_VAR)
  EAD$C_DEMAND <- C_DEMAND
  
  #### CUSTOMER DIVERSITY ####
  
  A_CCN =  .create_designmatrix(NUMB_C,NUMB_CN,DENS_CCN,"C","CN") #Customer - Customer Needs Matrix
  
  CN = C_DEMAND %*% (A_CCN)  #computing CN * q from the customers
  EAD$CN = CN
  
  
  #### MARKET STRUCTURE  ####
  
  A_CNFR = .create_designmatrix(NUMB_CN,NUMB_FR,DENS_CNFR,"CN","FR") #Customer Needs - Functional Requirements Matrix
  
  FR = as.vector(CN) %*% (A_CNFR)   # computing FR * q
  EAD$FR = FR
  
  
  #### PRODUCT ARCHITECTURE ####
  
  A_FRCM = .create_designmatrix(NUMB_FR,NUMB_CM,DENS_FRCM,"FR","CM") #Functional Requirements - Components Matrix
  
  #Number of functional requirements must be equal to the number of components. (symmetrical matrix needed)
  #  if(EAD$TYPE_FRCM != "C"){
  #  if(sum(diag(A_FRCM))<NUMB_FR)  {
  #   diag(A_FRCM) <- 1
  #   A_FRCM[!lower.tri(A_FRCM,diag=TRUE)] <- 0
  # }
  #  }
  EAD$DENS_FRCM_measured = count_nonzeros(A_FRCM) #set DENS_FRCM is not strictly the implemented. 
  
  CM = as.vector(FR) %*% (A_FRCM) # computing CM * q
  EAD$CM = CM
  
  #### PRODUCTION TECHNOLOGY ####   #A_CMPV wird anders gebildet in gen:ProductEnvironment
  
  
  EAD = gen_ProductionEnvironment(EAD,NUMB_CM,NUMB_PV,DENS_CMPV)
  A_CMPV = EAD$A_CMPV
  A_PVRC = .create_designmatrix(NUMB_PV,NUMB_RC,DENS_PVRC,"PV","RC") #Processed - Resources Matrix
  
  PV = as.vector(CM) %*% (A_CMPV) # computing CM * q
  EAD$PV = PV
  
  #### EAD COMPUTING ####
  
  RC = as.vector(PV) %*% (A_PVRC) # computing CM * q
  
  EAD$RC = RC
  
  RCC = matrix(.gen_RCC(RCC_VAR,TC,RC))
  #RCU = (RCC/RC)
  
  
  #### COST COMPUTING ####
  
  A_PVRCp <- sweep((A_PVRC),2,colSums(A_PVRC),"/") #Absolute matrix to relative matrix
  A_CMPVp <- sweep((A_CMPV),2,colSums(A_CMPV),"/") #Absolute matrix to relative matrix  
  A_FRCMp <- sweep((A_FRCM),2,colSums(A_FRCM),"/") #Absolute matrix to relative matrix
  A_CNFRp <- sweep((A_CNFR),2,colSums(A_CNFR),"/") #Absolute matrix to relative matrix
  A_CCNp  <- sweep((A_CCN),2,colSums(A_CCN),"/")   #Absolute matrix to relative matrix
  
  PVC =  (A_PVRCp) %*% as.vector(RCC)
  CMC =  (A_CMPVp) %*% as.vector(PVC)
  FRC =  (A_FRCMp) %*% as.vector((CMC))
  CNC =  (A_CNFRp) %*% as.vector((FRC))
  CC  =  (A_CCNp)  %*% as.vector((CNC))
  
  # Check routine 
  A_CRC = ((as.vector(C_DEMAND)) * A_CCN) %*% A_CNFR %*% A_FRCM %*% A_CMPV %*% A_PVRC
  EAD$RCDB = RCC / as.vector(RC) # computing the resource cost driver Benchmark
  EAD$CC = A_CRC  %*% EAD$RCDB #computing the total costs of each product
  EAD$CCB = EAD$CC / C_DEMAND # computing the unit costs of each product (customer costs)
  
  EAD$RCC = RCC
  EAD$A_CCN = A_CCN
  EAD$A_CNFR = A_CNFR
  EAD$A_FRCM = A_FRCM
  EAD$A_CMPV = A_CMPV
  EAD$A_PVRC = A_PVRC
    
  
return(EAD)}
