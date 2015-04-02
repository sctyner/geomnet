#Attempted Madmen Relationships

load('data/mm_directed.rda')

library(network)
library(sna)
library(ggplot2)

ggplot(data = mm.directed$edges, aes(from_id = Name1, to_id = Name2)) +
  geom_net(vertices = mm.directed$vertices, directed = T, vlabel = T,
           vsize = I(2.5), layout = 'fruchtermanreingold',
           aes(vcolour=c( "#FF69B4", "#0099ff")[as.numeric(Gender)])) +
  expand_limits(x = c(0,1.1), y = c(0,1)) + theme_net