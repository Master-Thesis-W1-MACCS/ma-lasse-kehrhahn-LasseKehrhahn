error_raiser <-
function(EAD){
  # 
  # ##Matrix Size Errors if uncoupled or decoupled matrix is wanted##
  # if(EAD$NUMB_C != EAD$NUMB_CN & (EAD$DENS_CCN == 2 | EAD$TYPE_CCN == "UC"| EAD$TYPE_CCN == "DC")){
  #   stop("Number of Customers is unequal to Number of Customer Needs: Symmetrical matrix can not be generated")}
  # 
  # if(EAD$NUMB_CN != EAD$NUMB_FR & (EAD$DENS_CNFR == 2 | EAD$TYPE_CNFR == "UC"| EAD$TYPE_CNFR == "DC")){
  #   stop("Number of Customers is unequal to Number of Functional Requirements: Symmetrical matrix can not be generated")}
  # 
  # if(EAD$NUMB_FR != EAD$NUMB_CM & (EAD$DENS_FRCM == 2 | EAD$TYPE_FRCM == "UC"| EAD$TYPE_FRCM == "DC")){
  #   stop("Number of Functional Requirements is unequal to Number of Componentes: Symmetrical matrix can not be generated")}
  # 
  # if(EAD$NUMB_CM != EAD$NUMB_PV & (EAD$DENS_CMPV == 2 | EAD$TYPE_CMPV == "UC"| EAD$TYPE_CMPV == "DC")){
  #   stop("Number of Components is unequal to Number of Processes: Symmetrical matrix can not be generated")}
  # 
  # if(EAD$NUMB_PV != EAD$NUMB_RC & (EAD$DENS_PVRC == 2 | EAD$TYPE_PVRC == "UC"| EAD$TYPE_PVRC == "DC")){
  #   stop("Number of Processes is unequal to Number of Resources: Symmetrical matrix can not be generated")}
  # 
  # ##Matrix Size Errors if coupled matrix is wanted##
  # if(EAD$TYPE_CCN == "C" & EAD$DENS_CCN == 2){
  #   stop("Diagonal Customer/Customer Needs Matrix can not be generated when the Matrix Type is coupled (C)")}
  # 
  # if(EAD$TYPE_CNFR == "C" & EAD$DENS_CNFR == 2){
  #   stop("Diagonal Customer Needs/Functional Requirements Matrix can not be generated when the Matrix Type is coupled (C)")}
  # 
  # if(EAD$TYPE_FRCM == "C" & EAD$DENS_FRCM == 2){
  #   stop("Diagonal Functional Requirements/Componentes Matrix can not be generated when the Matrix Type is coupled (C)")}
  # 
  # if(EAD$TYPE_CMPV == "C" & EAD$DENS_CMPV == 2){
  #   stop("Diagonal Components/Processes Matrix can not be generated when the Matrix Type is coupled (C)")}
  # 
  # if(EAD$TYPE_PVRC == "C" & EAD$DENS_PVRC == 2){
  #   stop("Diagonal Processes/Resources Matrix can not be generated when the Matrix Type is coupled (C)")}
  # 
  #Matrix Density Errors##
  # if(EAD$TYPE_CCN == "UC" & EAD$DENS_CCN != 2){
  #   stop("Type and Density Parameters are not matching for A_CCN")}
  # 
  # if(EAD$TYPE_CNFR == "UC" & EAD$DENS_CNFR != 2){
  #   stop("Type and Density Parameters are not matching for A_CNFR")}
  # 
  # if(EAD$TYPE_FRCM == "UC" & EAD$DENS_FRCM != 2){
  #   stop("Type and Density Parameters are not matching for A_FRCM")}
  # 
  # if(EAD$TYPE_CMPV == "UC" & EAD$DENS_CMPV != 2){
  #   stop("Type and Density Parameters are not matching for A_CMPV")}
  # 
  # if(EAD$TYPE_PVRC == "UC" & EAD$DENS_PVRC != 2){
  #   stop("Type and Density Parameters are not matching for PVRC")}
  
}
