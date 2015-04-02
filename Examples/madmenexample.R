#Madmen Relationships

load('data/madmen.rda')

library(network)
library(sna)
library(ggplot2)

ggplot(data = madmen$edges, aes(from_id = Name1, to_id = Name2)) +
  geom_net(vertices = madmen$vertices, vsize=3, vlabel= TRUE, ecolour="grey30",
           aes(vcolour=c( "#FF69B4", "#0099ff")[as.numeric(Gender)])) +
  expand_limits(x = c(0,1.25), y = c(0,1)) + theme_net
