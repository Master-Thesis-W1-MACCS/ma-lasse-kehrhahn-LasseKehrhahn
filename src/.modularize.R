#
.modularize <- function(EAD,NUMB_CN,NUMB_C,TQ) {
 
  
  
  ### 
 
  Modularize_FR_level = 2  #modularization based on medium market segment
  NUMB_M = 1               #because modularization happens over medium market segment, there is only one module generated


  #Referenzmatrix; 
  #FR1 = Eine CM ; FR2 = 2 CM ; FR = 3 CM //dadurch teurer -> Low, Mid, High
  #A_FRCM = matrix(c(1,0,0,0,1,1,1,1,1),nrow=EAD$NUMB_FR,ncol =EAD$NUMB_CM,byrow = TRUE)
  A_FRM = EAD$A_FRCM
  
  # A_FRCM1 = matrix(c(1,0,0,1,1,0,1,1,1),nrow=EAD$NUMB_FR,ncol =EAD$NUMB_CM,byrow = TRUE)  
  # colnames(A_FRCM1) = c('CM1','CM2','CM3')
  # rownames(A_FRCM1) = c('FR1','FR2','FR3')
  #A_FRM = A_FRCM          #functional requirements - components matrix ---> Functional requirements - modules 
  A_MPV = EAD$A_CMPV          #componentes - processes matrix ----> modules - processes 
  #A_FRM = A_FRCM
  #A_FRM = EAD$A_FRCM       #if we want to use standard matrix from gen_EAD
    
  #A_FRM = matrix(c(1,0,0,0,1,1,1,1,1),nrow=NUMB_FR,ncol =NUMB_CM,byrow = TRUE)  

  ## START MODULARIZATION
  #for (fr in seq(NUMB_FR)) {
  #pvs_module <- rep(0, EAD$NUMB_PV)
  # 1. GET THE FRAME FOR THE MODULE - FR2 
  cms_used_for_module = A_FRM[Modularize_FR_level,] #gives the 1 and 0 of row 2
  # 2. COMPOSITE COMPONENTS INTO ONE MODULE
  cms_used_for_module_idx = which((cms_used_for_module>0))   #
  #numbcms= sum(cms_used_for_module>0)
  
  #--------------#---------------
  # 3. MODULARIZE THE A_FRM MATRIX BY MERGING THE CMs TO THE M
  # 3.1 DEFINE WHICH COMPONENTS GO INTO A MODULE AND WHICH NOT
  cms_not_used_for_module_idx = setdiff(as.vector(unique(col(EAD$A_FRCM))),cms_used_for_module_idx)
  
  A_FRM_1 = as.matrix(A_FRM[,cms_not_used_for_module_idx])  #components that are not modularized
  #colnames(A_FRM_1) = paste0("CM",c(cms_not_used_for_module_idx))
  
  # 3.2. NEW EMPTY MATRIX THAT ONLY CONTAINS THE MODULE M
  
  A_FRM_2 = matrix(rep(0,EAD$NUMB_FR),nrow=EAD$NUMB_FR,ncol = NUMB_M ,byrow = TRUE) 
  A_FRM_2 = as.matrix(A_FRM[,NUMB_M])
  
  # 3.3 MERGE THE TWO COMPONENTS INTO ONE MODULE
  
  for (row in 1:nrow(A_FRM)){
    #When using the maximum value = Max(....)
    #When using the sum = Sum(...)
    A_FRM_2[row,] = max(A_FRM[row,cms_used_for_module_idx])       
    
  }

  # 3.4 BINDING THE TWO MATRICES TO GET A_FRM
  
  A_FRM = matrix(as.vector(cbind(A_FRM_2, A_FRM_1)),nrow = EAD$NUMB_FR, ncol = (ncol(EAD$A_FRCM)-length(cms_used_for_module_idx)+NUMB_M)) #+1 because we have one module
  
  colnames(A_FRM) = c('M1',rep(paste0('CM',c(1:length(cms_not_used_for_module_idx)))))
  rownames(A_FRM) = c(rep(paste0('FR',c(1:nrow(A_FRM)))))
  #
  #-------------------------A_MPV----------------------------
  # SAME PROCESS AS FOR A_FRM
  
  pvs_used_for_module_idx = ceiling(which(A_MPV[cms_used_for_module_idx,]>0)/nrow(A_MPV))  #test if that also works for more than three pvs
  
  pvs_not_used_for_module_idx = setdiff(as.vector(unique(col(EAD$A_CMPV))),pvs_used_for_module_idx)
  
  A_MPV_1 = A_MPV[pvs_not_used_for_module_idx,]
  
  A_MPV_2 = matrix(c(rep(0,EAD$NUMB_PV)),nrow=NUMB_M,ncol = EAD$NUMB_PV ,byrow = TRUE)  
  
  for (col in 1:ncol(A_MPV)){
    #When using the maximum value = Max(....)
    #When using the sum = Sum(...)
    A_MPV_2[,col] = max(A_MPV[pvs_used_for_module_idx,col])
    
  }
  
  A_MPV = matrix(as.vector(rbind(A_MPV_2,A_MPV_1)), nrow = (ncol(EAD$A_CMPV)-length(cms_used_for_module_idx)+NUMB_M),ncol = EAD$NUMB_PV)
  
  rownames(A_MPV) = c(colnames(A_FRM))
  colnames(A_MPV) = c(rep(paste0("PV",c(1:ncol(A_MPV)))))
  
  #----------------------A_PVRC--------------------------------

# 
#   for (pv in seq(NUMB_PV)){
#     # 3. GET THE PVS OF THE NEW MODULE
#         pvs_used_for_module = A_CMPV[cms_used_for_module_idx,] #Processes that can be joined together because of the modularization
#       }    
#     # 4. AGGREGATE THE PVS OVER THE MODULE
#       for (row in 1:nrow(pvs_used_for_module)) { # aggregation function for building a new production lines for the module. 
#         pvs_module = pvs_used_for_module[row,] + pvs_module
#         A_MPV[row,] = pvs_module
#          #5. CLEAN UP THE MATRIX
#         }
#   
#   for (row in nrow(A_MPV))  {
#   
#     A_MPV[row,] = pvs_module
#     A_MPV = A_MPV[-row,]  SS
#   }
# 
# 
# 
# 
# #DELETION OF PROCESSES THAT ARE NOT USED ANYMORE 3x3->2x3
#   
# A_MPV = A_MPV[,-which(colSums(A_MPV)==0)]
#   
# 
# 
#     #NEXT A_PVRC!!!
  
  
  
  EAD$A_FRM = A_FRM
  EAD$A_MPV = A_MPV
  #EAD$A_FRCM = A_FRCM1
  return(EAD)
  
}




calc_EAD <- function(EAD) {
  
  EAD$A_FRCM
  A_CRC_Modularized = ((as.vector(EAD$C_DEMAND)) * EAD$A_CCN) %*% EAD$A_CNFR %*% EAD$A_FRM %*% EAD$A_MPV %*% EAD$A_PVRC
  
  
  EAD$CCM_T = A_CRC_Modularized  %*% EAD$RCDB  #computing the total costs of each product
  EAD$CCM = EAD$CCM_T / EAD$C_DEMAND # computing the unit costs of each product  
  
  
  return(EAD)
  
}

