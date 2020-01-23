#####
## Extended Axiomatic Design   // 2020-01-02   V 0.01
##
## 0 - Install librairies - Library

# install.packages(c(
#   "dplyr",
#   "tidyr",
#   "rmarkdown",
#   "ggplot2",
#   "igraph"
# 
# ))
# 
# Packages <- c("dplyr", "ggplot2", "rmarkdown", "tidyr", "igraph")
# lapply(Packages, library, character.only = TRUE)


##############################
# 1 - Start
##############################

## SOURCE THIS FILE FOR EXECUTION
#source("01 INIT.R")
source("src/.modularize.R")
source("src/gen_EAD.R")
source("src/.designfunctions.R")
source("src/.gen_RCC.R")
source("src/.networkvisualization.R")

source("01 INIT.R")
