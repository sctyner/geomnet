#Tennis data
tennis <- read.csv("~/Desktop/NetworksResearch/Stats.csv")
dim(tennis)

head(tennis)
library(dplyr)
tennis %>% group_by(MatchID)

matches <- tennis[, c("MatchID", "Year", "Surface", "Tournament","Round","Result","Player", "Nationality_1", "Opponent", "Nationality_2",names(tennis)[grep('Total',names(tennis))])]
matches <- matches[-which(as.numeric(rownames(tennis)) %% 2 == 0),]
summary(matches)
length(levels(matches$Tournament))
levels(tennis$Result) <- c(0,1)
tennis$Result <- as.numeric(as.character(tennis$Result))
players <- data.frame(tennis %>% group_by(Player, Year, Tournament) %>% summarise(n.wins = sum(Result), total = n(), perc.win = n.wins/total))
names(players)[1] = 'label'

ggplot(data = na.omit(filter(matches, Year == 2014, Tournament == 'Wimbledon')), aes(from_id = Player, to_id = Opponent, ealpha = .2)) +
  geom_net(vertices = filter(players, Year == 2014, Tournament == 'Wimbledon'), layout= 'fruchtermanreingold', vlabel = T) + 
  expand_limits(x = 0:1, y = 0:1) + theme_net

ggplot(data = na.omit(filter(matches, Year == 2014, Tournament == 'US Open')), aes(from_id = Player, to_id = Opponent, esize = 2, ecolour = tennis.colors[as.numeric(factor(Round))])) +
  geom_net(vertices = filter(players, Year == 2014, Tournament == 'US Open'), layout= 'fruchtermanreingold', vlabel = T) + 
  expand_limits(x = 0:1, y = 0:1) + theme_net

ggplot(data = na.omit(filter(matches, Year == 2014, Tournament == 'Australian Open')), aes(from_id = Player, to_id = Opponent, esize = 2, ecolour = tennis.colors[as.numeric(factor(Round))])) +
  geom_net(vertices = filter(players, Year == 2014, Tournament == 'Australian Open'), layout= 'fruchtermanreingold', vlabel = T) + 
  expand_limits(x = 0:1, y = 0:1) + theme_net

library(RColorBrewer)
tennis.colors <- brewer.pal(7, "PuBu")
tennis.colors <- tennis.colors[c(7,6,5,4,3,2,1)]
