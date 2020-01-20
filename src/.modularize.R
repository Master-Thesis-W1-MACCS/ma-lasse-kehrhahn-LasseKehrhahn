#
.modularize <- function(EAD,NUMB_CN,NUMB_C,TQ) {
 
  OVERDESIGN = 3 
  
  
A_FRM = A_FRCM
A_MPV = A_CMPV
A_FRM  =  matrix( rep( 0, len=NUMB_FR*(NUMB_CM+1)), ncol = NUMB_CM+1)

#große neue MAtrix aufbauen 

A_FRM = matrix(c(1,1,0,0,0,0,1,0,0,0,0,1),nrow=NUMB_FR,ncol =NUMB_CM,byrow = TRUE)  




#Merging functional requirements or merging components 

#A_FRM[1,] = A_FRCM[1,] + A_FRCM[2,]
#A_FRM = A_FRM[-2,]




## START MODULARIZATION

#for (fr in seq(NUMB_FR)) {
  pvs_module <- rep(0, NUMB_PV)
  # 1. GET THE FRAME FOR THE MODULE - 
  cms_used_for_module = A_FRM[1,]
  # 2. COMPOSITE COMPONENTS INTO ONE MODULE
  cms_used_for_module_idx = which((cms_used_for_module>0))
  #numbcms= sum(cms_used_for_module>0)

  
  for (pv in seq(NUMB_PV)){
    # 3. GET THE PVS OF THE NEW MODULE
        pvs_used_for_module = A_CMPV[cms_used_for_module_idx,]
      }    
    # 4. AGGREGATE THE PVS OVER THE MODULE
      for (row in cms_used_for_module_idx) { # aggregation function for building a new production lines for the module. 
        pvs_module = pvs_used_for_module[row,] + pvs_module
        A_MPV[row,] = pvs_module
         #5. CLEAN UP THE MATRIX
        }
  
  for (row in cms_used_for_module_idx)  {
  
  A_MPV[row,] = pvs_module
  
  
  A_MPV = A_MPV[-row,]  
  }
 
  
  
  
  
 
#}


rm(A_FRM)

# buiFRlding module at FR1

A_FRM[,5] = A_FRCM[,1] + A_FRCM[,3] + A_FRCM[,4]

  
}
