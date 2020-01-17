#

gen_EAD <- function(EAD,NUMB_CN,NUMB_C,TQ) {
  
# Customer generation function
#  % 23.4. - Generates the customers in the market 
#  % 24.7. - Make customers' attributes more precise, in particular CN and FRN
#   01.10 - Code simplification 



CN <- c(rep(1, NUMB_CN))
CUSTOMERS <- c(10,20,30)
# Which customers exist and how many needs do they got? 
    
    # Customer.NEEDS 
    # Explanation; 
    
    #       CN1  CN2  CNm 
    # C1    1     1   1       [does have this necessity]  
    # C2    1     0   1       [does not have this necessity]
    # Cn    n     n   nm
    DENS_C = 2
    DENS_CNFR = -1 
    DENS_FRCM = -1
    DENS_CMPV = -1
    DENS_PVRC = -1
    
    NUMB_C =3
    NUMB_CN = 3 
    NUMB_FR = 4
    NUMB_CM = 5
    NUMB_PV = 6
    NUMB_RC = 50
    
    
    EAD = list()
    
    
    
    A_CCN =  .create_designmatrix(NUMB_C,NUMB_CN,DENS_C)
    A_CNFR = .create_designmatrix(NUMB_CN,NUMB_FR,DENS_CNFR)
    A_FRCM = .create_designmatrix(NUMB_FR,NUMB_CM,DENS_FRCM)
    A_CMPV = .create_designmatrix(NUMB_CM,NUMB_PV,DENS_CMPV)
    A_PVRC = .create_designmatrix(NUMB_PV,NUMB_RC,DENS_PVRC)
    
    CN = t(A_CCN)  %*% CUSTOMERS # computing CN * q from the customers
    FR = t(A_CNFR) %*% CN  # computing FR * q
    CM = t(A_FRCM) %*% FR  # computing CM * q
    PV = t(A_CMPV) %*% CM  # computing CM * q
    RC = t(A_PVRC) %*% PV  # computing CM * q
    
    




}

  