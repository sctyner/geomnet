
load('data/protein.rda')

#subset the data
sample.edges <- protein$edges[sample(nrow(protein$edges), 500),]
yeast.samp <- list(edges = sample.edges, 
                   vertices = data.frame(label = unique(c(sample.edges$from,sample.edges$to))))

library(network)
library(sna)
library(ggplot2)

ggplot(data = yeast.samp$edges, aes(from_id = from, to_id = to)) +
  geom_net(vertices = yeast.samp$vertices, ealpha = .25, valpha = .5, 
           vcolour = 'magenta', layout = 'random', 
           layout.par = list(dist = 'uniang')) +
  expand_limits(x = c(0,1), y = c(0,1)) + theme_net
