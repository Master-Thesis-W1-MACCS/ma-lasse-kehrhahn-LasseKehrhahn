#
.modularize <- function(EAD,NUMB_CN,NUMB_C,TQ) {
 
Modularize_FR_level = 2  #?
  
  
A_FRM = A_FRCM          #functional requirements - components matrix ---> Functional requirements - modules 
A_MPV = A_CMPV          #componentes - processes matrix ----> modules - processes 
A_FRM  =  matrix( rep(0, len=NUMB_FR*(NUMB_CM+1)), ncol = NUMB_CM+1) #?

#große neue MAtrix aufbauen 

A_FRM = matrix(c(1,0,0,0,1,1,1,1,1),nrow=NUMB_FR,ncol =NUMB_CM,byrow = TRUE)  #nur zwischenschritt?


## START MODULARIZATION

#for (fr in seq(NUMB_FR)) {
  pvs_module <- rep(0, NUMB_PV)
  # 1. GET THE FRAME FOR THE MODULE - FR2 
  cms_used_for_module = A_FRM[Modularize_FR_level,]
  # 2. COMPOSITE COMPONENTS INTO ONE MODULE
  cms_used_for_module_idx = which((cms_used_for_module>0))
  #numbcms= sum(cms_used_for_module>0)

  for (pv in seq(NUMB_PV)){
    # 3. GET THE PVS OF THE NEW MODULE
        pvs_used_for_module = A_CMPV[cms_used_for_module_idx,]
      }    
    # 4. AGGREGATE THE PVS OVER THE MODULE
      for (row in 1:nrow(pvs_used_for_module)) { # aggregation function for building a new production lines for the module. 
        pvs_module = pvs_used_for_module[row,] + pvs_module
        A_MPV[row,] = pvs_module
         #5. CLEAN UP THE MATRIX
        }
  
  for (row in nrow(A_MPV))  {
  
    A_MPV[row,] = pvs_module
    A_MPV = A_MPV[-row,]  
    
    
  }

  #NEXT A_PVRC!!!!
  #Arent resources just summed up as well?
  

  
}
