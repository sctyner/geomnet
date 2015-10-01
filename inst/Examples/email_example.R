#Email Network from http://vacommunity.org/VAST+Challenge+2014%3A+Mini-Challenge+1#Download_ the_Datasets_Entry_Forms_and_Documentation

load('data/email.rda')
email.net <- email$edges
employee <- email$nodes

library(network)
library(sna)
library(ggplot2)
library(RColorBrewer)
cols <- brewer.pal(8, name="Set1")

#no facets
ggplot(data = subset(email.net, nrecipients < 54), aes(from_id = From, to_id = to)) +
  geom_net(vertices = employee,ealpha = 0.1, vsize = I(4), directed = TRUE,
           aes(vcolour=cols[as.numeric(CurrentEmploymentType)])) +
  expand_limits(x=c(0,1), y = c(0,1)) + theme_net

#facet by day 
ggplot(data = subset(email.net, nrecipients < 54), aes(from_id = From, to_id = to)) +
  geom_net(vertices = employee,ealpha = 0.1, vsize = I(4), directed = TRUE,
           aes(vcolour=cols[as.numeric(CurrentEmploymentType)])) +
  expand_limits(x=c(0,1), y = c(0,1)) + facet_wrap(~day, nrow = 2) + theme_net