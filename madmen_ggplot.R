library(igraph)
library(network)
library(ggplot2)
library(sna)
library(ergm)
library(tergm)
library(grid)
library(gcookbook)

data(madmen2)
madmen2
mm.net <- network(madmen2, directed = FALSE, matrix.type = "edgelist")
network.vertex.names(mm.net)
sex <- c("m", "f", "m", "m", "f", "f", "f", "m", "f", "m", "f", "m", "m", "f", "m", "m", "f", "m", "m",
         rep("f",9), "m", "m", "m", "f", "f", "f", "f", "m", "f", "m", "f", "f", "m", "f", "m", "m", "f",
         "m", "f", "f", "f", "f", "f", "f")
set.vertex.attribute(mm.net, "group",  sex) #pkg: network

plotg(mm.net)
