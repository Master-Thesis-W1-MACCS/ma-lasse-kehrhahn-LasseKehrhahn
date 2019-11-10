



AC_CN =   matrix(c(1,0,0,1),nrow=2,ncol=2)
ACN_FR =  matrix(c(1,0,0,1),nrow=2,ncol=2)
AFR_CM =  matrix(c(1,0,1,0,0,1),nrow=2,ncol=3)
ACM_PV =  matrix(c(1,0,0,0,1,0,0,1,2,0,0,1),nrow=3,ncol=4)
APV_RC =  matrix(c(1,0,0,0,2,0,0,0,0,2,0,0,0,1,0,0,0,0,1,0,0,0,0,1),nrow=4,ncol=6)




C_RC = ACN_FR %*% AFR_CM %*% ACM_PV %*% APV_RC
C_RC

