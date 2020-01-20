#
gen_EAD <- function(EAD,NUMB_CN,NUMB_C,TQ) {
  
# Customer generation function
#  % 23.4. - Generates the customers in the market 
#  % 24.7. - Make customers' attributes more precise, in particular CN and FRN
#   01.10 - Code simplification 

CUSTOMERS <- vector()
CUSTOMERS <- c(10, 30, 50)


    DENS_C = 2
    DENS_CNFR = 2
    DENS_FRCM = -1
    DENS_CMPV = -1
    DENS_PVRC = -1
    
    NUMB_C = 3
    NUMB_CN = 3
    NUMB_FR = 3
    NUMB_CM = 4
    NUMB_PV = 5
    NUMB_RC = 6
    RC_VAR = -1
    
    
    A_CCN =  .create_designmatrix(NUMB_C,NUMB_CN,DENS_C,"C","CN")
    A_CNFR = .create_designmatrix(NUMB_CN,NUMB_FR,DENS_CNFR,"CN","FR")
    A_FRCM = .create_designmatrix(NUMB_FR,NUMB_CM,DENS_FRCM,"FR","CM")
    A_CMPV = .create_designmatrix(NUMB_CM,NUMB_PV,DENS_CMPV,"CM","PV")
    A_PVRC = .create_designmatrix(NUMB_PV,NUMB_RC,DENS_PVRC,"PV","RC")
    
    
    
    CN = CUSTOMERS %*% (A_CCN)  # computing CN * q from the customers
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

  