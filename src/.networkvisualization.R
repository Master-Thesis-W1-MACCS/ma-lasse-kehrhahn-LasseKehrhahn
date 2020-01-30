#install.packages('igraph')


.plotigraph <- function(A_CNFR,A_FRCM,A_CMPV,A_PVRC) {

g1 <-graph_from_incidence_matrix(A_CNFR, weighted = TRUE)
g2 <- graph_from_incidence_matrix(A_FRCM, weighted = TRUE)
g3 <-graph_from_incidence_matrix(A_CMPV, weighted = TRUE)
g4 <- graph_from_incidence_matrix(A_PVRC, weighted = TRUE)

g_sum = g1 + g2  + g3 + g4

g_sum.com <- fastgreedy.community(g_sum)
V(g_sum)$color <- g_sum.com$membership + 1

plot(g_sum)

}


