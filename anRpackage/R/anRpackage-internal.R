.benchmark <-
function(EAD,NUMB_CN,NUMB_C,TQ) {
  
  A_FRM = EAD$A_FRCM
  A_MPV = EAD$A_CMPV  
  
  
  
  EAD$A_FRM = A_FRM
  EAD$A_MPV = A_MPV
  
  return(EAD)
  
}
.create_designmatrix <-
function(X,Y,DENS,rowname="X",colname="Y") {
 #generating A_X_Y  => design matrix
  
  
repeat
  {

  if(DENS == 2) {                 #Density is defined in gen_EAD for each matrix separatly (e.g. DENS_CNFR), if density = 2 we want a diagonal matrix
    
    if (X!=Y) {A_XY = 'error'}    #Size must be identical nrow = ncol
    else {
        A_YX = diag(X)# full decoupled
      }
    
  }
  
  else if(DENS == -1) {           #if density = -1 we want a matrix with a random density between the desired boundaries (eg. [0.4,0.7])
    DENS_MIN = 0.4
    DENS_MAX = 0.7
    DENS = runif(1, DENS_MIN, DENS_MAX);
    rand_DENS = runif(X*Y) #draw random numbers
    A_YX = matrix(ifelse(rand_DENS > DENS, 0,1),nrow=X,ncol=Y) ## 1/0 DENSITY 
  
  }
  
  else {                          #if density is set to a fixed value 
    
    rand_DENS = runif(X*Y) #draw random numbers
    A_YX = matrix(ifelse(rand_DENS > DENS, 0,1),nrow=X,ncol=Y) ## 1/0 DENSITY 
  
  }

  ROW_ZEROS<-any(rowSums(A_YX[,])==0)   #every product need at least one resource
  COL_ZEROS<-any(colSums(A_YX[,])==0)   #every resource needs to be used at least once
  
  if(ROW_ZEROS==FALSE & COL_ZEROS==FALSE) {  break  }   
  }
  

  
rownames(A_YX) = c(paste0(rowname, 1:nrow(A_YX)))
colnames(A_YX) = c(paste0(colname, 1:ncol(A_YX)))

return(A_YX)
 
}
.gen_Demand <-
function(NUMB_C,TQ,Q_VAR){
  # This has been used in the Mertens (2020) for modeling dispersed realized demand

  if (Q_VAR == -1)
  {
    Q_VAR_MIN = 0.4
    Q_VAR_MAX = 1.6
    Q_VAR = runif(1, Q_VAR_MIN, Q_VAR_MAX) #runif = Uniform Distribution on interval min to max.
  }
  
  preDemand = rlnorm(NUMB_C, meanlog = 1, sdlog = Q_VAR) #preDemand is buildup as a -> Log Normal Distribution whose logarithm has mean equal to meanlog and standard deviation equal to sdlog
  DEMAND = ceiling((preDemand/sum(preDemand))*TQ) #ceiling = runden
  EAD$Q_VAR_draw = Q_VAR
  
  #CHECKS 
  Qs = sort(DEMAND, decreasing = TRUE)
  EAD$CHECK$Q20 = sum(Qs[1:(0.2 * NUMB_C)])/TQ        #no. of units of 20% biggest products
  
  
  
  return(DEMAND)
}
.gen_RCC <-
function(RC_VAR, TC, NUMB_RC)  
  {
  # INIT
  if (RC_VAR == -1)
  {
    RC_VAR_MIN = 0.4
    RC_VAR_MAX = 0.7
    RC_VAR = runif(1, RC_VAR_MIN, RC_VAR_MAX)
    RC_VAR = RC_VAR
  }
  
   preRCC = rlnorm(NUMB_RC, meanlog = 1, sdlog = RC_VAR)
   RCC = (preRCC/sum(preRCC))*TC #normalizing it #ceiled realized demand for each product
  
    ## Move the biggest resource to the front
  largest_RC <-
    sort(RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  RCC <- c(RCC[largest_RC], RCC[-largest_RC])
  
  ###CHECK###
  RCCs = sort(RCC, decreasing = TRUE)
  
  RCC20 = sum(RCCs[1:(0.2 * length(RCC))])/TC     #size of 20% biggest resources
  RCC10 = sum(RCCs[1:(0.1 * length(RCC))])/TC     #size of 10% biggest resources
  RCC02 = sum(RCCs[1:(0.02 * length(RCC))])/TC    #size of 2% biggest resources
  
  #plot(sort(RCC))
  
  #### sourcing

  return(RCC)
  
}
.gen_RES_CONS_PAT <-
function(EAD,NUMB_CM,NUMB_PV,DENS_CMPV){

## ====================== STEP 1 Determining the activities =========================
  UNITLEVEL_ACT_SHARE_MIN = 0.2    #0.2 is the size of DISP1 =10
  UNITLEVEL_ACT_SHARE_MAX = 0.4
  EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE = runif(1, UNITLEVEL_ACT_SHARE_MIN, UNITLEVEL_ACT_SHARE_MAX) #random activity share between lower and upper bounds
  
  EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE_MIN = UNITLEVEL_ACT_SHARE_MIN
  EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE_MAX = UNITLEVEL_ACT_SHARE_MAX
## ====================== STEP 1 Determining the amount of cost categories =================
  
  unitsize = floor(EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE*NUMB_PV)
  nonunitsize = NUMB_PV-unitsize
  
  EAD$PRODUCTION_TECHNOLOGY$UNITSIZE = unitsize
  EAD$PRODUCTION_TECHNOLOGY$NONUNITSIZE = nonunitsize

## ====================== STEP 0.b Determining the density (DENS)  =========================

  #Randomization and setting clear design points. 

  if(DENS_CMPV == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX)}
  else{DENS=DENS_CMPV}
    EAD$PRODUCTION_TECHNOLOGY$DENS_CMPV = DENS

## ====================== STEP 1 BASELINE NORM ========================= 

repeat    {
    
BASE = rnorm(NUMB_PV) #creates for every CO (product) a random number
  
RES_CONS_PATpre = matrix(rnorm(NUMB_CM*NUMB_PV,mean=0,sd=1), 
                         NUMB_CM, NUMB_PV)                            #random pre matrix, as Baseline

RES_CONS_PAT = matrix(0, nrow = NUMB_CM, ncol = NUMB_PV, byrow = TRUE) #empy matrix, that is going to be filled 


## ====================== STEP 1.a CORRELATION ========================= 
# Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
# Rows Products Colums Resources

COR1 =-1
COR2 =-1

# Correlation of  resources
if(COR1 == -1){
  COR1 <- runif(1, -0.2, 0.8)
 
}

sqrt_const_1 <- sqrt(1 - (COR1 * COR1))

# Correlation of the remaining resources
if(COR2 == -1){
  COR2 <- runif(1, -0.2, 0.8)
  
}

sqrt_const_2 <- sqrt(1 - (COR2 * COR2))

for (i in 1:(EAD$PRODUCTION_TECHNOLOGY$UNITLEVEL_ACT_SHARE*NUMB_PV)) #unitsize+1
{
  RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,i];
}

for (i in ((unitsize)+1) : NUMB_PV) #nonunitsize+1 (34+1)
{
  RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,i];
}

## ====================== STEP 1.b DENSITY ========================= 
res_cons_pat_b_pre = runif(NUMB_CM*NUMB_PV)

## 1/0 DENSITY
res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                          NUMB_CM,NUMB_PV)


RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PAT = RES_CONS_PAT

## ====================== STEP 1.c Ceiling and Scaling ============= 

# take absolute value of X and Z and scale by 10 and round them
# Anand et al. 2019
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
RES_CONS_PAT[,1] <- (BASE)
RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)

##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
RES_CONS_PAT_TOTAL <- RES_CONS_PAT * as.vector(EAD$CM)



##CALCULATING TCU
TCU <- colSums(RES_CONS_PAT_TOTAL)
##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix

## ===================== EXCPETION HANDLER ====================

# EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019 # It is important the the first RES_CONS_PAT column has no zeros
# in accordance with Anand etl. 2019 and Balakrishnan et al. 2011; Substantiation of this hidden formalization remains unclear. 

PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource
RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?

if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
{
  break
}

}


## ====================== STEP 3 CHECK ========================= 

# AverageZeroConsumption
  EAD$PRODUCTION_TECHNOLOGY$NonZeroConsumption = sum(colSums(RES_CONS_PAT != 0))/     #Ratio of Zeros in Res_cons_pat
  (NUMB_CM * NUMB_PV)

# Average consumption of products consuming a resource
EAD$PRODUCTION_TECHNOLOGY$countNonZero<-mean(colSums(RES_CONS_PAT[,]>0))

# Correlation Test
# EAD$PRODUCTION_TECHNOLOGY$COR1<-mean(cor(RES_CONS_PAT[,1:(unitsize)])[1,])
  
# EAD$PRODUCTION_TECHNOLOGY$COR2<-mean(cor(RES_CONS_PAT[,c(1,((unitsize)+1):NUMB_PV)])[1,])

rownames(RES_CONS_PAT) = c(paste0('CM', 1:nrow(RES_CONS_PAT)))
colnames(RES_CONS_PAT) = c(paste0('PV', 1:ncol(RES_CONS_PAT)))


#Average distance 
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PAT = RES_CONS_PAT
EAD$A_CMPV = RES_CONS_PAT
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
EAD$PRODUCTION_TECHNOLOGY$RES_CONS_PATp = RES_CONS_PATp


#
#
#
return(EAD)


}
.modularize <-
function(EAD,NUMB_CN,NUMB_C,TQ) {
 
 
  Modularize_FR_level = 1  #modularization based on medium market segment
  NUMB_M = 1               #because modularization happens over medium market segment, there is only one module generated

  
  A_FRM = EAD$A_FRCM
  A_MPV = EAD$A_CMPV      

  ## START MODULARIZATION
  # 1. GET THE FRAME FOR THE MODULE - FR2 
  cms_used_for_module = A_FRM[Modularize_FR_level,] 
  # 2. COMPOSITE COMPONENTS INTO ONE MODULE
  cms_used_for_module_idx = which((cms_used_for_module>0))   
  
  #--------------#---------------
  # 3. MODULARIZE THE A_FRM MATRIX BY MERGING THE CMs TO THE M
  # 3.1 DEFINE WHICH COMPONENTS GO INTO A MODULE AND WHICH NOT
  cms_not_used_for_module_idx = setdiff(as.vector(unique(col(EAD$A_FRCM))),cms_used_for_module_idx)

  A_FRM_1 = as.matrix(A_FRM[,cms_not_used_for_module_idx])  #components that are not modularized
  
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
  
  A_FRM = matrix(as.vector(cbind(A_FRM_2, A_FRM_1)),nrow = EAD$NUMB_FR, ncol = (ncol(EAD$A_FRCM)-length(cms_used_for_module_idx)+NUMB_M))
  if (length(cms_not_used_for_module_idx)==0) {colnames(A_FRM) = c('M1')}
  else {colnames(A_FRM) = c('M1',rep(paste0('CM',c(1:length(cms_not_used_for_module_idx)))))}
  
  rownames(A_FRM) = c(rep(paste0('FR',c(1:nrow(A_FRM)))))
  
  #
  #-------------------------A_MPV----------------------------
  # SAME PROCESS AS FOR A_FRM
  # pvs_used_for_module_idx = (which(A_MPV[cms_used_for_module_idx,]>0))  #test if that also works for more than three pvs
 
  
  if (length(cms_used_for_module_idx)<=1){
    pvs_used_for_module_idx = as.numeric(A_MPV[cms_used_for_module_idx]!= 0)
  } else {
    pvs_used_for_module_idx = colSums(A_MPV[cms_used_for_module_idx,]!= 0)
  }
 
  pvs_used_for_module_idx= which(pvs_used_for_module_idx!=0,arr.ind = T)
  pvs_not_used_for_module_idx = setdiff(as.vector(unique(col(EAD$A_CMPV))),pvs_used_for_module_idx)
  
  A_MPV_2 = matrix(c(rep(0,EAD$NUMB_PV)),nrow=NUMB_M,ncol = EAD$NUMB_PV ,byrow = TRUE)  

  for (col in 1:ncol(A_MPV)){
    #When using the maximum value = Max(....)
    #When using the sum = Sum(...)
    A_MPV_2[,col] = max(A_MPV[cms_used_for_module_idx,col])
    
  }
  
  A_MPV_1 = A_MPV[-c(cms_used_for_module_idx),] # delete components of the module 
  A_MPV = matrix(nrow = (nrow(EAD$A_CMPV)-length(cms_used_for_module_idx)+NUMB_M),ncol = EAD$NUMB_PV)
  
  ### this function does not work right. !
  A_MPV = matrix(as.vector(rbind(A_MPV_2,A_MPV_1)), nrow = (nrow(EAD$A_CMPV)-length(cms_used_for_module_idx)+NUMB_M),ncol = EAD$NUMB_PV)
  rownames(A_MPV) = c(colnames(A_FRM))
  colnames(A_MPV) = c(rep(paste0("PV",c(1:ncol(A_MPV)))))

    

  
  EAD$A_FRM = A_FRM
  EAD$A_MPV = A_MPV
  #print(A_FRM)
  #print(A_MPV)
  return(EAD)
  
}
.plotigraph <-
function(A_CNFR,A_FRCM,A_CMPV,A_PVRC) {

g1 <-graph_from_incidence_matrix(A_CNFR, weighted = TRUE)
g2 <- graph_from_incidence_matrix(A_FRCM, weighted = TRUE)
g3 <-graph_from_incidence_matrix(A_CMPV, weighted = TRUE)
g4 <- graph_from_incidence_matrix(A_PVRC, weighted = TRUE)

g_sum = g1 + g2  + g3 + g4
#g_sum = simplify(g_sum)
g_sum.com <- fastgreedy.community(g_sum)
V(g_sum)$color <- g_sum.com$membership + 1

plot(g_sum)

}
.product_datalogging <-
function(o,nn,EAD,DATAp){
 
  
  ####### DOES NOT WORK YET ############
  
  
  PRODUCT <- c(PRODUCT, 1:NUMB_PRO) #How many products per run 
  DENS[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DENS #Scaling firm parameter to products.
  Q_VAR[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$Q_VAR #Scaling firm parameter to products.
  RCC_VAR[PRODUCT] = FIRM$COSTING_SYSTEM$RC_VAR #Scaling firm parameter to products.
  CP[PRODUCT] = FIRM$COSTING_SYSTEM$CP #Scaling firm parameter to products.
  Error[PRODUCT] = FIRM$COSTING_SYSTEM$Error #Scaling firm parameter to products.
  NUMB_Error[PRODUCT] = FIRM$COSTING_SYSTEM$NUMB_Error #Scaling firm parameter to products.
  CC[PRODUCT] = FIRM$COSTING_SYSTEM$CC
  MISCPOOLSIZE[PRODUCT] = FIRM$COSTING_SYSTEM$MISCPOOLSIZE
  RUN[PRODUCT] = o #run, repetition
  DESIGN[PRODUCT] = nn #which kind of design? 
  
  
  PE = (FIRM$COSTING_SYSTEM$PCH - FIRM$COSTING_SYSTEM$PCB)/FIRM$COSTING_SYSTEM$PCB
  APE = abs((FIRM$COSTING_SYSTEM$PCH - FIRM$COSTING_SYSTEM$PCB))/FIRM$COSTING_SYSTEM$PCB
  PCb[PRODUCT] = FIRM$COSTING_SYSTEM$PCB
  PCh[PRODUCT] = FIRM$COSTING_SYSTEM$PCH
  Q[PRODUCT] = FIRM$PRODUCTION_ENVIRONMENT$DEMAND
  
  DATApre = data.frame(o,nn,PRODUCT,PCb,PCh,Q,PE,APE,DENS,Q_VAR,RCC_VAR,CP,Error,NUMB_Error,CC,MISCPOOLSIZE) # construct the dataframe 
  
  DATAp = rbind(DATAp,DATApre) #put it together
  
  return(DATAp)
}
.Random.seed <-
c(10403L, 567L, -1838684365L, -77726319L, -650281897L, -34161300L, 
-1104642099L, 2061366512L, 769556160L, 1235247642L, -2034243620L, 
602010368L, 394227885L, 2082143783L, 60425431L, -532110464L, 
444460460L, 124395769L, 63396945L, -807649025L, 1434618859L, 
578924927L, -299371797L, 1614374860L, -1485064605L, -831008919L, 
-1933890392L, -253272619L, 1102855550L, -1695719625L, 1458483522L, 
-1460270788L, 271300523L, -1316426937L, 1960651511L, -944881793L, 
-1307683564L, -1702208677L, 813168548L, -1312597122L, -34584954L, 
30253826L, 1610274562L, -1848458807L, -1763947689L, 121400355L, 
1537367197L, -63110354L, 11855517L, 328130479L, -757145972L, 
1501607637L, 714301144L, 1389335954L, -1195509035L, -532105626L, 
1507915332L, 393738784L, 2027488806L, -955453168L, -1630905941L, 
1421229252L, -924191871L, 2120289226L, -1344558760L, -605812616L, 
1629311382L, 1958609509L, -982004554L, -343008200L, -614482747L, 
1223207118L, 1006143837L, -1399681578L, 1604699843L, -424269500L, 
-1637131824L, -892279779L, -1292429623L, 186652583L, -578101350L, 
-101100939L, 1954943720L, -1147849938L, 1119893832L, 1378396970L, 
-1970865954L, -571452188L, -268852253L, 81538499L, 1546973073L, 
836077159L, 1478532617L, 2107671483L, 652876087L, 586589L, 340320437L, 
-2136987528L, -517237012L, 699051913L, 1322853400L, -1462507963L, 
729664910L, 174849284L, 2085479436L, 531086439L, 655688291L, 
172095895L, -1105310769L, -1352697213L, -65779506L, 1462012392L, 
188324762L, -429120947L, -1879482574L, -1562324710L, -1794306663L, 
33392049L, 499839262L, 330806764L, -862809934L, -1034041411L, 
-286111064L, 2040095579L, -2053989320L, 1573861808L, 743175641L, 
1815887002L, 1260733327L, 1568706082L, -1193319535L, 196067039L, 
-848562689L, -583954722L, 1107110657L, -75799717L, -1418739649L, 
1502681632L, -1741299429L, -1673528518L, 1795385459L, -1597432103L, 
-1928115834L, 1480807381L, 1125025989L, 780386370L, 1632009197L, 
-1262261015L, 597523769L, 1970527256L, -752121359L, 561761799L, 
340440837L, -1709001800L, 1217756884L, -1838585013L, 548884302L, 
1860916772L, 272745925L, 1573445515L, -1091999466L, 338531115L, 
-832115016L, 765419L, -150879862L, 990714887L, -1774131503L, 
-1113180935L, 682829944L, -903650997L, -285302647L, 1316622433L, 
795768719L, -1315064381L, 211891328L, 1631714843L, 1513652565L, 
-1570566526L, 560220543L, -157646747L, -433115865L, 1800869670L, 
1149754829L, -1975202274L, -964282438L, 1409878850L, -112215533L, 
616272561L, -1612129528L, 1633618944L, 1103210841L, 1641702901L, 
816225215L, 740958185L, 225838410L, -261589075L, 1939033805L, 
61458151L, 1273246421L, -2041501483L, -1633533290L, 1847563690L, 
288838829L, -930923625L, -1857856232L, -608827L, 269883630L, 
-1524285016L, -1782073898L, 1967063244L, 2241520L, -1826583574L, 
1311620054L, -283134841L, 1065962799L, -1209651365L, 1344358949L, 
-1823431659L, -1233660710L, -906921857L, -1011375290L, 311842158L, 
2099603022L, -1578303225L, 702646809L, -1514526207L, -2088853187L, 
376743956L, -628423551L, 8160942L, 962051723L, -1061242228L, 
-1624889768L, 1245307637L, -1404662693L, 1164060263L, 182624893L, 
-932014225L, 421929744L, 1199723405L, -4194567L, 421243818L, 
-933397909L, 1334602288L, -1176735705L, -1245012482L, 627129468L, 
-1347469256L, -1862902384L, 1848623907L, 540418920L, 1280977982L, 
1398074372L, -1506460975L, 1348102637L, -2010116709L, 1608175730L, 
-692879119L, 2145164000L, -1249555440L, 1681595675L, 976428337L, 
-1445195755L, -1465496269L, -1674593329L, 755772613L, -137679371L, 
-1077559753L, 1376606950L, 343513105L, 1462731450L, -1790483123L, 
1461653025L, -1466603222L, -1359501L, -781828497L, 537157818L, 
-640984066L, -1129807521L, -341310487L, 1014074538L, -188198964L, 
919272783L, 475116982L, 798416293L, 1714769549L, 107197356L, 
-1408303187L, 1869695344L, 1644365679L, -626075145L, -729338191L, 
1072684751L, 1821471727L, -1241329983L, 2035559937L, -1379388524L, 
-910742269L, 1755191080L, -419626366L, -1675086535L, -1907518196L, 
-2142247911L, 1314440353L, -1817664565L, 468232682L, 472609861L, 
1740772829L, -294240784L, 1157237779L, -2025515679L, 35516307L, 
-1196039148L, -1877738203L, 1525500926L, -1535192661L, -362633732L, 
320514870L, 152799763L, -891734774L, -670522192L, 356203510L, 
483678682L, 165527429L, -1673352131L, -1512761088L, -1384990054L, 
-671229127L, -1057977915L, -1823113375L, -750841419L, 875627997L, 
-1570757802L, -154663262L, -330514627L, 1137359743L, 1039931499L, 
612499262L, -519722488L, -894104297L, 410742074L, 1946348351L, 
1421728450L, 517826581L, 1235128236L, 285903977L, -739389605L, 
-1894811142L, -1048874539L, -559507040L, 1121081583L, 1623644438L, 
892274849L, -1024623027L, -1464648088L, -796582586L, -1736817063L, 
-1143454209L, -1070117392L, 660673921L, -565570881L, -1337047257L, 
-734105538L, -239758566L, 2113897396L, 934150882L, 1583118291L, 
2043249053L, 492387776L, 102001036L, 260181687L, 2112476996L, 
-1292002516L, -1860358435L, -107655657L, -301071275L, -2098884011L, 
1195215098L, 1480452517L, -16326561L, -1184670444L, -718756405L, 
1885259942L, 1311656695L, 1277400777L, 251520363L, 1657662169L, 
-1849747388L, 789563860L, -375977814L, 478312777L, 1631325486L, 
872937393L, -972349898L, 1988232601L, -175963469L, -1770174160L, 
2056674353L, -1309352082L, 282475504L, -426060631L, 776921422L, 
-1064489695L, -497325192L, 323710032L, 215043686L, 579913243L, 
-1868652484L, -1888811038L, -139106415L, -705508602L, -1632082108L, 
247226399L, -1335705789L, -1534130193L, -428238525L, -744466132L, 
867384629L, 1366729025L, 1683369931L, 1922533960L, 1115438524L, 
825220573L, -1116940788L, -2025564107L, -287555173L, 12210572L, 
611444916L, -1451333006L, 618523447L, -906754346L, 1287400116L, 
-1217690091L, 1815652580L, -1596133446L, -1837067496L, -342052464L, 
-105130197L, 416203995L, -451050724L, -2139671743L, 192556389L, 
-1509545122L, 847097384L, -1301599579L, -2046534758L, 812038911L, 
603595168L, -975593057L, -1481872643L, -1039550183L, -1339378431L, 
-544968146L, 1106669610L, 10811577L, 1331642144L, -2048017849L, 
-1309272479L, -914111994L, 798150067L, -1910693457L, 983710378L, 
-526307256L, 1100751204L, -1152753281L, -75237062L, -294166024L, 
381950488L, 862934120L, -1009460001L, -1527425596L, -237279422L, 
-274590196L, -1273584560L, 485083975L, -889898065L, 1278968669L, 
-395521796L, 1186198498L, -172958286L, -302148795L, -909052325L, 
-1895028678L, 1579961778L, 1240554838L, 1236407617L, -116083262L, 
-1585856880L, -661720709L, -1535849320L, -887226959L, -1175230028L, 
1027346435L, 1901727925L, -1321830463L, 632571422L, 2062229430L, 
-588886713L, -309162482L, -1814090691L, 1698283542L, 1808954859L, 
376691769L, -1343788157L, 1326971369L, 2106458287L, 202834527L, 
-677711062L, 507989373L, -682908875L, -963075516L, 214827298L, 
-391833735L, -1754427738L, -1708058004L, 1088715L, 1443905603L, 
944079926L, 330181985L, 723572234L, -1732288882L, 1712884840L, 
-292598744L, -1276085076L, 1751414818L, -141815940L, -1054948491L, 
873270416L, -340282209L, 1292781035L, 1426634005L, 1632918979L, 
909436482L, -1225729558L, -826827730L, -1062087193L, 799552995L, 
-1487003428L, -1234406181L, -690163777L, 2093264015L, 1780837433L, 
-70941362L, -1194987448L, 683031230L, 1717960886L, 1267696913L, 
-1041787731L, -1400127518L, -1525202455L, -31639686L, -632572264L, 
119824237L, -1234322765L, 1564778850L, 1090506257L, -1563192816L, 
-690231434L, -1533057380L, 633815531L, -1721960410L, 1810107151L, 
-2057996104L, 2116072490L, 1768511798L, 1800755403L, 2046531992L, 
651120647L, 1103609004L, 159085060L, -1703581985L, 968076844L, 
2067388351L, -1564019488L, -904041576L, 1670967497L, 784308248L, 
-991821473L, 212325539L, 590009006L, -2066484177L, 1444446383L, 
-1491709858L, -1157866528L, -94196208L, -1845140798L, -1898957194L, 
-1839507855L, 1796531657L, 1561627812L, 957673506L, -1759312626L, 
-1599460313L, -1700848398L, 364055076L, -1781285307L, 1223363020L, 
1380428098L, 858500262L, 2114162546L, -216477335L, -1485586114L, 
1448305641L, 1355228980L, -1966818618L, 929330203L, -1010563720L, 
1224787821L, 754125228L, 134334561L, -1070539211L, -780433575L, 
-1712304267L, 1644261748L, 483499940L, 1468205807L, -648462674L, 
1171323543L, 497721953L, -1446149414L, 264122117L, 455935904L, 
-399338845L, -1982973179L, 2059998882L, 567254056L)
.system_datalogging <-
function(o,nn,EAD,DATA){
 # browser()
  
  

DATApre = data.frame(o,nn,EAD$NUMB_C, EAD$NUMB_CN, EAD$NUMB_FR, EAD$NUMB_CM, EAD$NUMB_PV, EAD$NUMB_RC,
                       EAD$DENS_CCN, EAD$DENS_CNFR, EAD$DENS_FRCM, EAD$DENS_CMPV, EAD$DENS_PVRC,
                       EAD$Q_VAR, EAD$RCC_VAR, EAD$DENS_FRCM_measured,
                       EAD$Diff_total)
  
colnames(DATApre) = c('o','nn','NUMB_C','NUMB_CN', "NUMB_FR", "NUMB_CM", "NUMB_PV", "NUMB_RC",
                     "DENS_CCN", "DENS_CNFR","DENS_FRCM","DENS_CMPV","DENS_PVRC",
                     "Q_VAR","RCC_VAR", "DENS_FRCM_m",
                     "DIFF_COST")    
 
DATA = rbind(DATA,DATApre) #put it together
 

  return(DATA)
}
.visNetwork <-
function(A_CCN,A_CNFR,A_FRCM,A_CMPV,A_PVRC) {
  
  
  #1.RESHAPING THE MATRICES
  colname = "X"
  
  #Reshaping A_CCN
  CCN = data.frame(cbind(A_CCN,rownames(A_CCN)))
  CCN = melt(CCN, id = paste0("V",ncol(CCN)))
  CCN = CCN[CCN$value ==1,]
  colnames(CCN) = c(paste0(colname, 1:ncol(CCN)))
  
  #Reshaping A_CNFR
  CNFR = data.frame(cbind(EAD$A_CNFR,rownames(A_CNFR)))
  CNFR = melt(CNFR, id = paste0("V",ncol(CNFR)))
  CNFR = CNFR[CNFR$value ==1,]
  colnames(CNFR) = c(paste0(colname, 1:ncol(CNFR)))
  
  #Reshaping A_FRCM
  FRCM = data.frame(cbind(A_FRCM,rownames(A_FRCM)))
  FRCM = melt(FRCM, id = paste0("V",ncol(FRCM)))
  FRCM = FRCM[FRCM$value ==1,]
  colnames(FRCM) = c(paste0(colname, 1:ncol(FRCM)))
  
  #Reshaping A_CMPV
  CMPV = data.frame(cbind(A_CMPV,rownames(A_CMPV)))
  CMPV = melt(CMPV, id = paste0("V",ncol(CMPV)))
  CMPV = CMPV[CMPV$value ==1,]
  colnames(CMPV) = c(paste0(colname, 1:ncol(CMPV)))
  
  #Reshaping A_PVRC
  PVRC = data.frame(cbind(A_PVRC,rownames(A_PVRC)))
  PVRC = melt(PVRC, id = paste0("V",ncol(PVRC)))
  PVRC = PVRC[PVRC$value ==1,]
  colnames(PVRC) = c(paste0(colname, 1:ncol(PVRC)))
  
  #2.MERGING THE JUST REHSAPED MATRICES TO ONE DATAFRAME
  
  x1 = merge(CCN,CNFR, by.x = "X2",by.y = "X1")
  x1 = data.frame(x1$X1,x1$X2,x1$X2.y)
  colnames(x1) = c("C","CN","FR")
  
  x2 = merge(x1,FRCM, by.x = "FR",by.y = "X1")
  x2 = data.frame(x2$C,x2$CN,x2$FR,x2$X2)
  colnames(x2) = c("C","CN","FR","CM")
  
  x3 = merge(x2,CMPV, by.x = "CM",by.y = "X1")
  x3 = data.frame(x3$C,x3$CN,x3$FR,x3$CM,x3$X2)
  colnames(x3) = c("C","CN","FR","CM","PV")
  
  x4 = merge(x3,PVRC, by.x = "PV",by.y = "X1")
  x4 = data.frame(x4$C,x4$CN,x4$FR,x4$CM,x4$PV,x4$X2)
  colnames(x4) = c("C","CN","FR","CM","PV","RC")
  
  network = data.frame(x4)
  
  #3.TRANSFORM THE DATAFRAME INTO A DATA TREE for collapsible Tree####
  
  # network$pathString = paste("EAD",
  #                            network$C,
  #                            network$CN,
  #                            network$FR,
  #                            network$CM,
  #                            network$PV,
  #                            network$RC,
  #                            sep = "/")
  # 
  # EAD_Tree = as.Node(network)
  # 
  # collapsibleTree(EAD_Tree)
  
  
  
  #4.VISNETWORK####
  
  nodes = data.frame(as.vector(unique(unlist(network))),
                     as.vector(unique(unlist(network))),
                     sub("^([[:alpha:]]*).*", "\\1",as.vector(unique(unlist(network)))))
  
  colnames(nodes) = c("id","label","group")
  
  
  e1 = data.frame(network$C,network$CN)
  colnames(e1) = c("from","to")
  e2 = data.frame(network$CN,network$FR)
  colnames(e2) = c("from","to")
  e3 = data.frame(network$FR,network$CM)
  colnames(e3) = c("from","to")
  e4 = data.frame(network$CM,network$PV)
  colnames(e4) = c("from","to")
  e5 = data.frame(network$PV,network$RC)
  colnames(e5) = c("from","to")
  
  edges = rbind(e1,e2,e3,e4,e5)
  
  edges = unique(edges[,1:2])
  
  #Layout options
  smooth = c(rep("FALSE",length(edges$from)))
  
  edges = cbind(edges, smooth)
  
  visNetwork(nodes,edges)%>%
    visOptions(highlightNearest = TRUE) %>% 
    visLayout(improvedLayout = TRUE)
  
}
