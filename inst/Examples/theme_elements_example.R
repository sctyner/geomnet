#Theme Inheritance structure in ggplot2: see bottom of this page: http://docs.ggplot2.org/current/theme.html

load('data/theme_elements.rda')
names(theme_elements$vertices)[1] <- 'label'

library(network)
library(sna)
library(ggplot2)


ggplot(data = theme_elements$edges, aes(from_id = parent, to_id = child)) +
  geom_net(vertices = theme_elements$vertices, directed = TRUE, vlabel = TRUE) + 
  expand_limits(x = c(0,1.2), y = c(0,1)) + theme_net
