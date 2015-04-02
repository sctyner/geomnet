#College Football Games in the Fall 2000 regular season
# Source: http://www-personal.umich.edu/~mejn/netdata/

load('data/football.rda')
football$vertices$label <- as.factor(football$vertices$label)
football$vertices$value <- as.factor(football$vertices$value)
football$edges$in.conf <- 0
for (i in 1:nrow(football$edges)){
  idx1 <- which(football$vertices$label == football$edges$from[i])
  idx2 <- which(football$vertices$label == football$edges$to[i])
  if (football$vertices$value[idx1] == football$vertices$value[idx2]){
    football$edges$in.conf[i] <- 1
  }
}

library(network)
library(sna)
library(ggplot2)
library(RColorBrewer)
colors <- brewer.pal(12, name = 'Paired')

ggplot(data = football$edges, aes(from_id = from, to_id = to,
                                  elinetype = in.conf + 1)) +
  geom_net(vertices = football$vertices, ealpha = 0.3, vsize = 2, vlabel = TRUE,
           aes(vcolour=colors[as.numeric(factor(value))])) +
  expand_limits(x = c(0,1), y = c(0,1)) + theme_net
