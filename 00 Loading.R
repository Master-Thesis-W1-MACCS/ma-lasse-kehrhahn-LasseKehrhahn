#####
## Extended Axiomatic Design   // 2020-01-02   V 0.01
# install.packages(c(
#   "dplyr",
#   "tidyr",
#   "rmarkdown",
#   "ggplot2",
#   "igraph",
#   "visNetwork",
#   "data.tree",
#   "reshape",
#   "roxygen2",
#   "devtools"
# 
# ))writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# 
# Packages <- c("dplyr", "ggplot2", "rmarkdown", "tidyr", "igraph","data.tree", "reshape",'visNetwork')
# lapply(Packages, library, character.only = TRUE)

## 0 - Install librairies - Library
# library(anRpackage)

##############################
# 1 - Start
##############################

#help with: ?FUNCTIONNAME (without ".")

## SOURCE THIS FILE FOR EXECUTION
#source("01 INIT.R")
source("src/.datalogging.R")                #egal
source("src/.auxiliar_functions.R")         #Hilfsfunktionen egal
source("src/.modularize.R")                 #Modularisierung später
source("src/gen_EAD.R")                     #Matrix Erstellung
source("src/.designfunctions.R")            #erstellt Matrizen mit .create_desginmatrix, wird auch in gen_EAD aufgerufen
source("src/.gen_RCC.R")                    #Erstellt Gesamtkostenmatrix .... ? Auch in gen_EAD
source("src/.networkvisualization.R")       #egal
source("src/gen_ProductionEnvironment.R")   #erstellt A_CMPV irgendwie anders als designfunctions mittels RES_CONS_PAT Auch in gen_EAD
source("src/.gen_RES_CONS_PAT.R")           #erstellt auch A_CMPV, wird in gen_ProductEnvironment aufgerufen .... ?
source("src/.gen_Q.R")                      #Demand generation ... ? Wird in gen_EAD ausgeführt
source("01 INIT.R")
