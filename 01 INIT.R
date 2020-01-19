#############################################################
# EXPLORATION OF THE EXTENDED AXIOMATIC DEIGN


## ============ CONTROL&FIXED PARAMETERS ===========
EAD = list()                           
DATA = data.frame()
DATAp = data.frame()


NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
NUMB_RES  =        50                     #INPUT independent variable - Number of factors
NUMB_CM =          10
SIM_NUMB =         200                    #Control Variable - Number of Simulations for every single environment (standard: 30)     

TC =               1000000                #Total costs
TQ =               100
NUMB_C =           3
NUMB_CN =          3
NUMB_FR_MAX =      3

## ============ INPUT PARAMETER MASK ===========
DENS_CNFR = c(-1)
DENS_FRCM = c(-1)
DENS_CMPV = c(-1)
DENS_PVRC = c(-1)  

Q_VAR = c(0.4)  
RCC_VAR =    c(-1)                           #Resource cost variation --> base for DISP2 (ABL2019) (0.2)

set.seed(13) #Reproducability
o=1 # First design point

## ==== DESIGN OF EXPERIMENTS ==== 
## EVIRONMENTAL FACTORS [] 
for (ix_DENS_CNFR in seq_along(DENS_CNFR)) {
  for (ix_DENS_FRCM in seq_along(DENS_FRCM)) {
    for (ix_DENS_CMPV in seq_along(DENS_CMPV)) {
      for (ix_DENS_PVRC in seq_along(DENS_PVRC)) {
        for (ix_Q_VAR in seq_along(Q_VAR)) {
          for (ix_RCC_VAR in seq_along(RCC_VAR)) {
                 
                    ## ====================== PREDETERMINING AND PREALLOCATION  =========================          
                    
                    EAD$DENS_CNFR = DENS_CNFR[ix_DENS_CNFR]   
                    EAD$DENS_FRCM = DENS_FRCM[ix_DENS_FRCM]   
                    EAD$DENS_CMPV = DENS_CMPV[ix_DENS_CMPV]   
                    EAD$DENS_PVRC = DENS_PVRC[ix_DENS_PVRC]   
                    EAD$Q_VAR = Q_VAR[ix_Q_VAR]
                    EAD$RCC_VAR = RCC_VAR[ix_RCC_VAR]
                    
                    
                    EAD$NUMB_PRO = NUMB_PRO
                    EAD$NUMB_RES = NUMB_RES
                    EAD$NUMB_C   = NUMB_C
                
                    nn=1 # necessary for repeating the SIM_NUMB loop
                    
                    #### ============================== SIMULATION ======================================
                    for (nn in 1:SIM_NUMB) {
                    
                    # COMPUTING THE BENCHMARK PRODUCT PROGRAM PLAN THROUGH THE EAD
                      
                    # Customer_generation [ NUMB_C ]
  
                    CUSTOMER = gen_CUSTOMER
                    
                      
                    # DATA = .system_datalogging(o,nn,FIRM,DATA)
                    
                      o=o+1 #Counting for the total number of runs
                    }
                  }
                }
              }  
            }  
          }
        }

#### ====================================== OUTPUT WRITING ===================================

#output data
# output = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
# write.csv(DATA, file = output)


# check = aggregate(DATA,list(DATA$CP),mean)
# plot(check$MAPE,type ='l')
# print(check$MAPE)

if (ProductCostOutput==1)
{
  output = paste("output/EAD_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")          
  write.csv(DATAp, file = output)
  print("Product costs FILE has been written")
}
