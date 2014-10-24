#example data
all <- paste(sort(rep(c('A', "B", 'O', "AB"),2)), rep(c("+","-"),4), sep = '')
A <- all[1:2]
B <- all[5:6]
AB <- all[3:4]
O <- all[7:8]

On <- cbind(from = rep('O-',8), to = all, group = 'O')
Op <- cbind(from = rep('O+',4), to = c(AB[1], A[1], B[1],O[1]), group = 'O')
Bn <- cbind(from = rep('B-',4), to = c(AB, B), group = 'B')
Bp <- cbind(from = rep('B+',2), to = c(AB[1], B[1]), group = 'B')
An <- cbind(from = rep('A-',4), to = c(AB, A), group = 'A')
Ap <- cbind(from = rep('A+',2), to = c(AB[1], A[1]), group = 'A')
ABn <- cbind(from = rep('AB-',2), to = AB, group = 'AB')
ABp <- cbind(from = 'AB+', to = 'AB+', group = 'AB')

blood.edges <- data.frame(rbind(ABn, ABp, An, Ap, Bn, Bp, On, Op))[,1:2]
blood.nodes <- unique(data.frame(rbind(ABn, ABp, An, Ap, Bn, Bp, On, Op))[,c(1,3)])
names(blood.nodes) <- c("type", "group")
blood.nodes$in.degree <- c(4,8,2,4,2,4,1,2)
blood.nodes$out.degree <- c(2,1,4,2,4,2,8,4)

#ggplot() + geom_net(nodes = blood.nodes, edges = blood.edges, 
#                     aes(vlabel = type, vcolor = group, vsize = in.degree))
