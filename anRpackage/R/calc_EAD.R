calc_EAD <-
function(EAD) {
  
  EAD$A_FRCM
  A_CRC_Modularized = ((as.vector(EAD$C_DEMAND)) * EAD$A_CCN) %*% EAD$A_CNFR %*% EAD$A_FRM %*% EAD$A_MPV %*% EAD$A_PVRC
  
  
  EAD$CCM_T = A_CRC_Modularized  %*% EAD$RCDB  #computing the total costs of each product
  EAD$CCM = EAD$CCM_T / EAD$C_DEMAND # computing the unit costs of each product  
  
  
  return(EAD)
  
}
