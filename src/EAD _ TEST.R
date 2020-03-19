


C = matrix(c(100,100,100),nrow=3,ncol=1)
AC_CN =   matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
ACN_FR =  matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
AFR_CM =  matrix(c(1,1,0,1,1,0,1,1,1,0,1,1,0,0,1,0,0,1),nrow=3,ncol=6)
ACM_PV =  matrix(c(1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1),nrow=6,ncol=3)
APV_RC =  matrix(c(0,1,0,1,0,0,0,1,1,0,1,1),nrow=3,ncol=4)




C_RC = (as.vector(C) * ACN_FR) %*% AFR_CM %*% ACM_PV %*% APV_RC
C_RC











C_RC = (as.vector(C) * ACN_FR) %*% AFR_CM %*% ACM_PV %*% APV_RC
C_RC
