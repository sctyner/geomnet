#Testing of the geom!

all <- paste(sort(rep(c('A', "B", 'O', "AB"),2)), rep(c("+","-"),4), sep = '')
A <- all[1:2]
B <- all[5:6]
AB <- all[3:4]
O <- all[7:8]

On <- cbind(from = rep('O-',8), to = all, letter = 'O')
Op <- cbind(from = rep('O+',4), to = c(AB[1], A[1], B[1],O[1]), letter = 'O')
Bn <- cbind(from = rep('B-',4), to = c(AB, B), letter = 'B')
Bp <- cbind(from = rep('B+',2), to = c(AB[1], B[1]), letter = 'B')
An <- cbind(from = rep('A-',4), to = c(AB, A), letter = 'A')
Ap <- cbind(from = rep('A+',2), to = c(AB[1], A[1]), letter = 'A')
ABn <- cbind(from = rep('AB-',2), to = AB, letter = 'AB')
ABp <- cbind(from = 'AB+', to = 'AB+', letter = 'AB')

blood <- data.frame(rbind(ABn, ABp, An, Ap, Bn, Bp, On, Op))
type <- c('AB','A','B','O')

blood.edges <- blood[-which(blood$from == blood$to),]
blood.vertices <- data.frame(node_id=unique(c(as.character(blood$from), as.character(blood$to))))

#node_id is a required aesthetic. #i have yet to match it with the nodes data in the geom code.
ggplot(data = blood.edges, aes(from_id= from, to_id = to)) + geom_net(vertices = blood.vertices) + expand_limits(x = -5, y = -1)


# #try again: 1/19
# 
# #get network coords first, outside of geom
# net <- as.network(blood.edges, matrix.type = "edgelist") #from network package
# m <- as.matrix.network.adjacency(net)
# #change the stuff below later to include multiple layouts (in if else statements?)
# vert.coord <- data.frame(sna::gplot.layout.kamadakawai(m, NULL)) # from sna package
# vert.labels <- as.factor(unique(c(as.character(blood.edges$from),as.character(blood.edges$to))))
# vert.coord$name <- sort(vert.labels) #needed here?
# names(vert.coord) <- c('x','y','node_id')
# edgelist <- as.matrix.network.edgelist(net)
# edge.coord <- data.frame(vert.coord[edgelist[,1],], vert.coord[edgelist[,2],])
# names(edge.coord) <- c('x','y','from_id','xend','yend','to_id')
# 
# edge.coord
# vert.coord
# 
# ggplot(vert.coord,aes(x=x,y=y)) + geom_net(edges = edge.coord) + expand_limits(x = -5, y = -5)
# 



