#### EXTENDED AXIOMATIC DESIGN ####
#### V. 1.00 - Stablized; Runable for many settings; 

## ============ CONTROL&FIXED PARAMETERS ===========
EAD = list()                           
DATA = data.frame()
DATAp = data.frame()

EAD$NUMB_C =       3
EAD$NUMB_CN =      3
EAD$NUMB_FR =      3
EAD$NUMB_PV =      3
EAD$NUMB_RC =      3

SIM_NUMB =         1000                 #Control Variable - Number of Simulations for every single environment (standard: 30)     

TC =               100                  #Total costs
TQ =               1                    #Total demand


###### STRUKTUR BEACHTUNG ##########
# EAD$TYPE_CCN  =    "UC"               #DefiNS = 0-1)
# EAD$TYPE_CNFR =    "UC"
# EAD$TYPE_FRCM =    "C"
# EAD$TYPE_CMPV =    "UC"
# EAD$TYPE_PVRC =    "UC"


## ==== INPUT PARAMETER MASK ===========
DENS_CCN = c(2)
DENS_CNFR = c(2)
DENS_FRCM = c(0.5)
DENS_CMPV = c(0.4)
DENS_PVRC = (0.5)
Q_VAR = c(-1)
RCC_VAR = c(-1)  #Resource cost variation --> base for DISP2 (ABL2019) (0.2)
NUMB_CM = c(3)


set.seed(15) #Reproducability
o=1 # First design point

## ==== DESIGN OF EXPERIMENTS ==== 

## ENVIRONMENTAL FACTORS [] 
for (ix_DENS_CCN in seq_along(DENS_CCN)) {
  for (ix_DENS_CNFR in seq_along(DENS_CNFR)) {
    for (ix_DENS_FRCM in seq_along(DENS_FRCM)) {
      for (ix_DENS_CMPV in seq_along(DENS_CMPV)) {
        for (ix_DENS_PVRC in seq_along(DENS_PVRC)) {
          for (ix_Q_VAR in seq_along(Q_VAR)) {
            for (ix_RCC_VAR in seq_along(RCC_VAR)) {
              for (ix_NUMB_CM in seq_along(NUMB_CM)) {
                 
                    ## ======== PREDETERMINING AND PREALLOCATION ==========          
                    EAD$NUMB_CM = NUMB_CM[ix_NUMB_CM]
                    EAD$DENS_CCN = DENS_CCN[ix_DENS_CCN]
                    EAD$DENS_CNFR = DENS_CNFR[ix_DENS_CNFR]   
                    EAD$DENS_FRCM = DENS_FRCM[ix_DENS_FRCM]   
                    EAD$DENS_CMPV = DENS_CMPV[ix_DENS_CMPV]   
                    EAD$DENS_PVRC = DENS_PVRC[ix_DENS_PVRC]   
                    EAD$Q_VAR = Q_VAR[ix_Q_VAR]
                    EAD$RCC_VAR = RCC_VAR[ix_RCC_VAR]
                    
                
                    nn=1 # necessary for repeating the SIM_NUMB loop
                    
                    ## ======== SIMULATION =========
                    for (nn in 1:SIM_NUMB) {
                      
                      
                      #error_raiser(EAD)  
                      
                      # COMPUTING THE BENCHMARK PRODUCT PROGRAM PLAN THROUGH THE EAD
                      EAD = gen_EAD(EAD,TQ)
                      
                      #EAD = .modularize(EAD,NUMB_CN,NUMB_C,TQ)
                      EAD = .benchmark(EAD,NUMB_CN,NUMB_C,TQ)
                      
                      EAD = calc_EAD(EAD)
                      
                      
                      EAD$Diff_unit =  EAD$CCM - EAD$CCB
                      EAD$Diff_total = sum(EAD$CCM_T- EAD$CC)
                      #print(EAD$DENS_FRCM)
                      #print(EAD$DENS_CMPV)
                      #print(EAD$Diff_total)
                      print(nn)
                      
                      #.plotigraph(EAD$A_CNFR,EAD$A_FRCM,EAD$A_CMPV,EAD$A_PVRC)
                      #.plotigraph(EAD$A_CNFR,EAD$A_FRM,EAD$A_MPV,EAD$A_PVRC)
                      #browser()
                      #.visNetwork(EAD$A_CCN,EAD$A_CNFR,EAD$A_FRCM,EAD$A_CMPV,EAD$A_PVRC)
                      #.visNetwork(EAD$A_CCN,EAD$A_CNFR,EAD$A_FRM,EAD$A_MPV,EAD$A_PVRC)
                      
                      
                      DATA = .product_datalogging(o,nn,EAD,DATA)
                      
                      o=o+1 #Counting for the total number of runs
                     }
                    }
                  }
                }
              }
            }  
          }  
        }
      }

## ==== OUTPUT WRITING ===================================

#output data
output = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(DATA, file = output)
print("FILE has been written")
