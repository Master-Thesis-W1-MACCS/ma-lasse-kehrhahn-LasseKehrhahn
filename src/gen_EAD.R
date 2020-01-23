#
gen_EAD <- function(EAD,NUMB_CN,NUMB_C,TQ) {
  
#
DENS_CCN =  EAD$DENS_CCN
DENS_CNFR = EAD$DENS_CNFR
DENS_FRCM = EAD$DENS_FRCM
DENS_CMPV = EAD$DENS_CMPV
DENS_PVRC = EAD$DENS_PVRC


browser()
NUMB_C = EAD$NUMB_C #Customers
NUMB_CN = EAD$NUMB_CN #Customer' needs 
NUMB_FR = EAD$NUMB_FR #Functional Requirements
NUMB_CM = EAD$NUMB_CM #Components
NUMB_PV = EAD$NUMB_PV #Processes
NUMB_RC = EAD$NUMB_RC #Resources
RC_VAR = -1 #Cost Variation among resource costs
DENS_C = 2
DENS_CNFR = 2
DENS_FRCM = 1
DENS_CMPV = 2
DENS_PVRC = 1
  
  
# Customer generation function
#  % 23.4. - Generates the customers in the market 
#  % 24.7. - Make customers' attributes more precise, in particular CN and FRN
#   01.10 - Code simplification 

CUSTOMERS <- vector()
CUSTOMERS <- c(10, 30, 50)



    A_CCN =  .create_designmatrix(NUMB_C,NUMB_CN,DENS_C,"C","CN") #Customer - Customer Needs Matrix
    A_CNFR = .create_designmatrix(NUMB_CN,NUMB_FR,DENS_CNFR,"CN","FR") #Customer Needs - Functional Requirements Matrix
    A_FRCM = .create_designmatrix(NUMB_FR,NUMB_CM,DENS_FRCM,"FR","CM") #Functional Requirements - Components Matrix
    A_CMPV = .create_designmatrix(NUMB_CM,NUMB_PV,DENS_CMPV,"CM","PV") #Components - Processed Matrix
    A_PVRC = .create_designmatrix(NUMB_PV,NUMB_RC,DENS_PVRC,"PV","RC") #Processed - Resources Matrix
    
    
    
    CN = CUSTOMERS %*% (A_CCN)  #computing CN * q from the customers
    FR = as.vector(CN) %*% (A_CNFR)   # computing FR * q
    CM = as.vector(FR) %*% (A_FRCM) # computing CM * q
    PV = as.vector(CM) %*% (A_CMPV) # computing CM * q
    RC = as.vector(PV) %*% (A_PVRC) # computing CM * q
    
    RCC = matrix(.gen_RCC(RC_VAR,1*10^6,RC))
    #RCU = (RCC/RC)
    

    
    
    ##############################################
    
    A_PVRCp <- sweep((A_PVRC),2,colSums(A_PVRC),"/") #Absolute matrix to relative matrix
    A_CMPVp <- sweep((A_CMPV),2,colSums(A_CMPV),"/") #Absolute matrix to relative matrix  
    A_FRCMp <- sweep((A_FRCM),2,colSums(A_FRCM),"/") #Absolute matrix to relative matrix
    A_CNFRp <- sweep((A_CNFR),2,colSums(A_CNFR),"/") #Absolute matrix to relative matrix
    A_CCNp  <- sweep((A_CCN),2,colSums(A_CCN),"/") #Absolute matrix to relative matrix
    
    PVC =  (A_PVRCp) %*% as.vector(RCC)
    CMC =  (A_CMPVp) %*% as.vector(PVC)
    FRC =  (A_FRCMp) %*% as.vector((CMC))
    CNC =  (A_CNFRp) %*% as.vector((FRC))
    CC  =  (A_CCNp)  %*% as.vector((CNC))
    C=sum(CC)  
    print(C)
}

  