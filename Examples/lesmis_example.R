# Les Misérables example
# Source: http://www-personal.umich.edu/~mejn/netdata/

load('data/lesmis.rda')

library(network)
library(sna)
library(ggplot2)

ggplot(data = lesmis$edges, aes(from_id = from, to_id = to, esize = value/mean(value))) +
  geom_net(vertices = lesmis$vertices, ealpha = 7/10,vcolour = I("red"), vsize = I(2.5), layout = 'fruchtermanreingold', vlabel=TRUE) +
  expand_limits(x = c(0,1.1), y = c(0,1)) + theme_net