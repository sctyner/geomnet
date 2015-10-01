#soccer data from Di.
library(RColorBrewer)

soccer <- read.csv("~/Desktop/NetworksResearch/boysDataClean.csv")

teams1 <- soccer[,c(1,3)]
teams2 <- soccer[,c(8,7)]
names(teams2) <- names(teams1)
teams <- rbind(teams1, teams2)
library(dplyr)
teams <- unique(teams) %>% arrange(desc(div), team)
names(teams)[2] <- 'label'

games <- soccer[, c(2:7, 9:12)]
head(games)
unique(games$date)
names(games)[c(2,6)] <- c("home", "away")

to.switch <- subset(games, ha == 'Away')
to.keep <- subset(games, ha!= 'Away')
head(to.switch)
switched <- to.switch[,c(1,6,3:5,2,7:10)]
head(switched)
names(switched)[c(2,6)] <- c("home", "away")
levels(switched$result)[c(2,4)] <- c("W","L")

games2 <- rbind(to.keep, switched)

games2 %<>% arrange(desc(season), week, date, home)

to.keep %<>% arrange(desc(season), week, date, home)

to.keep$same_div <- 1

for (i in 1:nrow(to.keep)){
  idx1 <- which(as.character(teams$label) == as.character(to.keep$home[i]))
  idx2 <- which(as.character(teams$label) == as.character(to.keep$away[i]))
  if (teams$div[idx1] == teams$div[idx2]){
    to.keep$same_div[i] <- 0
  }
}

 soccer.games <- to.keep

library(ggplot2)
cols <- brewer.pal(3, "Set1")
ggplot(data = soccer.games, aes(from_id = home, to_id = away, elinetype = same_div + 1, ealpha = .5)) + 
  geom_net(vertices = teams, aes(vcolour = cols[as.numeric(factor(div))]))+ 
  expand_limits(x = 0:1, y = 0:1) + facet_wrap(~season) + 
  theme_net
#above, 3A is green, 2A is blue, 1A is red 

soccer <- list(edges = soccer.games, vertices = teams)
#save(soccer, file = "~/Desktop/NetworksResearch/ggnet/soccer.rda")
