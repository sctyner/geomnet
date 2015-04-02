#Blood Donation

load('data/blood.rda')

library(network)
library(sna)
library(ggplot2)

ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
  geom_net(vertices = blood$vertices, vcolour = I('red'), layout = 'circle',
           vlabel = TRUE, vsize = I(3), directed = TRUE) +
  expand_limits(x = c(0,1), y = c(0,1)) + theme_net

