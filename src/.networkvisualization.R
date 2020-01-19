install.packages('igraph')
library(igraph)



rownames(A_CNFR) = c(paste0("CN", 1:nrow(A_CNFR)))
colnames(A_CNFR) = c(paste0("FR", 1:ncol(A_CNFR)))

g1 <-graph_from_incidence_matrix(A_CNFR, weighted = TRUE)

rownames(A_FRCM) = c(paste0("FR", 1:nrow(A_FRCM)))
colnames(A_FRCM) = c(paste0("CM", 1:ncol(A_FRCM)))


g2 <- graph_from_incidence_matrix(A_FRCM, weighted = TRUE)
plot(g2)

rownames(A_CMPV) = c(paste0("CM", 1:nrow(A_CMPV)))
colnames(A_CMPV) = c(paste0("PV", 1:ncol(A_CMPV)))

g3 <-graph_from_incidence_matrix(A_CMPV, weighted = TRUE)


rownames(A_PVRC) = c(paste0("PV", 1:nrow(A_PVRC)))
colnames(A_PVRC) = c(paste0("RC", 1:ncol(A_PVRC)))


g4 <- graph_from_incidence_matrix(A_PVRC, weighted = TRUE)





g_sum = g1 + g2  + g3 + g4

g_sum.com <- fastgreedy.community(g_sum)
V(g_sum)$color <- g_sum.com$membership + 1



plot(g_sum)


