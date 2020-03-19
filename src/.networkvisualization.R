#install.packages('igraph')


.plotigraph <- function(A_CNFR,A_FRCM,A_CMPV,A_PVRC) {

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


.visNetwork <- function(A_CCN,A_CNFR,A_FRCM,A_CMPV,A_PVRC) {
  
  
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


