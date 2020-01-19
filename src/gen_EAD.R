#

gen_EAD <- function(EAD,NUMB_CN,NUMB_C,TQ) {
  
# Customer generation function
#  % 23.4. - Generates the customers in the market 
#  % 24.7. - Make customers' attributes more precise, in particular CN and FRN
#   01.10 - Code simplification 

CUSTOMERS <- vector()
CUSTOMERS <- c(10,20,30,40,50)
# Which customers exist and how many needs do they got? 
    
    # Customer.NEEDS 
    #       CN1  CN2  CNm 
    # C1    1     1   1       [does have this necessity]  
    # C2    1     0   1       [does not have this necessity]
    # Cn    n     n   nm
    DENS_C = 2
    DENS_CNFR = 0.1
    DENS_FRCM = 0.1
    DENS_CMPV = 0.1
    DENS_PVRC = 0.1
    
    NUMB_C = 5
    NUMB_CN = 5
    NUMB_FR = 5
    NUMB_CM = 5
    NUMB_PV = 5
    NUMB_RC = 5
    RC_VAR = -1
    
    A_CCN =  .create_designmatrix(NUMB_C,NUMB_CN,DENS_C)
    
    
    A_CNFR = .create_designmatrix(NUMB_CN,NUMB_FR,DENS_CNFR)
    
    A_FRCM = .create_designmatrix(NUMB_FR,NUMB_CM,DENS_FRCM)
    
    
    
    
    A_CMPV = .create_designmatrix(NUMB_CM,NUMB_PV,DENS_CMPV)
    A_PVRC = .create_designmatrix(NUMB_PV,NUMB_RC,DENS_PVRC)
    
    
    CN = (A_CCN)  %*% CUSTOMERS # computing CN * q from the customers
    FR = (A_CNFR) %*% CN  # computing FR * q
    CM = (A_FRCM) %*% FR  # computing CM * q
    PV = (A_CMPV) %*% CM  # computing CM * q
    RC = (A_PVRC) %*% PV  # computing CM * q
    
    RCC = .gen_RCC(RC_VAR,1*10^6,RC)
    RCU = (RCC/RC)
    

    
    
    
    
    
    
    
    
    ##############################################
    
    A_PVRCp <- sweep((A_PVRC),2,colSums(A_PVRC),"/") #Absolute matrix to relative matrix
    A_CMPVp <- sweep((A_CMPV),2,colSums(A_CMPV),"/") #Absolute matrix to relative matrix  
    A_FRCMp <- sweep((A_FRCM),2,colSums(A_FRCM),"/") #Absolute matrix to relative matrix
    A_CNFRp <- sweep((A_CNFR),2,colSums(A_CNFR),"/") #Absolute matrix to relative matrix
    A_CCNp <- sweep((A_CCN),2,colSums(A_CCN),"/") #Absolute matrix to relative matrix
    
    PVC =  A_PVRCp %*% RCC 
    CMC =  A_CMPVp %*% PVC
    FRC =  A_FRCMp %*% CMC
    CNC =  A_CNFRp %*% FRC
    CC  =  A_CCNp  %*% CNC 
   
    
}

  