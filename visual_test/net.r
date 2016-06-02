vcontext("net")

data(blood)
set.seed(20140501)
p <- ggplot(data = blood$edges, aes(from_id = from, to_id = to))
p + geom_net(vertices=blood$vertices, aes(colour=..type..)) + theme_net()
save_vtest("Blood example, basic network in single data set")

bloodnet <- merge(blood$edges, blood$vertices, by.x="from", by.y="label", all=TRUE)
p <- ggplot(data = bloodnet, aes(from_id = from, to_id = to))
p + geom_net()
save_vtest("Blood example basic net")

p + geom_net(aes(colour=rho)) + theme_net()
save_vtest("Blood example nodes colored by rho factor")

p + geom_net(aes(colour=rho), label=TRUE, vjust = 0.5, hjust = 0.5, size = 10,
             labelcolour = "grey10")
save_vtest("Blood example nodes colored by rho factor, labelled")

p + geom_net(aes(colour=rho), label=TRUE, vjust = 0.5, hjust = 0.5, size = 10,
             labelcolour = "grey10", arrowgap =0.05,
             directed=TRUE, curvature=0.2) + theme_net()
save_vtest("Blood example nodes colored by rho factor, labelled, curved edges")

p + geom_net(aes(size=Predominance, colour=rho, shape=rho, linetype=group_to),
             linewidth=0.75, label =TRUE, labelcolour="black", size = 10,
             vjust = 0.5, hjust = 0.5) +
    facet_wrap(~Ethnicity) +
    scale_colour_brewer(palette="Set2") +
  theme_net()
save_vtest("Blood example, facetted")


gg <- ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
  geom_net(colour = "darkred", layout = "circle", label = TRUE, size = 15,
         directed = TRUE, vjust = 0.5, labelcolour = "grey80",
         arrowsize = 1.5, linewidth = 0.5, arrowgap = 0.05,
         selfies = TRUE, ecolour = "grey40") +
  theme_net()
gg
save_vtest("Blood example, with self references")


#'
#Madmen Relationships
data(madmen)
MMnet <- merge(madmen$edges, madmen$vertices, by.x="Name1", by.y="label", all=TRUE)
p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
p + geom_net(label=TRUE)
p + geom_net(aes(colour=Gender), size=6, linewidth=1, label=TRUE, fontsize=3, labelcolour="black")
p + geom_net(aes(colour=Gender), size=6, linewidth=1, label=TRUE, labelcolour="black") +
    scale_colour_manual(values=c("#FF69B4", "#0099ff")) + xlim(c(-.05,1.05))
p + geom_net(aes(colour=Gender), size=6, linewidth=1, directed=TRUE, label=TRUE,
             arrowgap=0.01, labelcolour="black") +
    scale_colour_manual(values=c("#FF69B4", "#0099ff")) + xlim(c(-.05,1.05))
#'
p <- ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2))
# alternative labelling: specify label variable.
p + geom_net(aes(colour=Gender, label=Gender), size=6, linewidth=1, fontsize=3,
             labelcolour="black")
#'
## visualizing ggplot2 theme elements
data(theme_elements)
TEnet <- merge(theme_elements$edges, theme_elements$vertices, by.x="parent",
               by.y="name", all=TRUE)
ggplot(data = TEnet, aes(from_id = parent, to_id = child)) +
  geom_net(label=TRUE, vjust=-0.5)
#'
#'
## emails example from VastChallenge 2014
# care has to be taken to make sure that for each panel all nodes are included with
# the necessary information.
# Otherwise line segments show on the plot without nodes.
#'
data(email)
employee <- data.frame(expand.grid(
              label=unique(email$nodes$label), day=unique(email$edges$day)))
employee <- merge(employee, email$nodes, by="label")
emailnet <- merge(subset(email$edges, nrecipients < 54), employee,
                  by.x=c("From", "day"), by.y=c("label", "day"), all=TRUE)
#'
#no facets
ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
  geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5) +
  scale_colour_brewer(palette="Set2")
#'
#facet by day
ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
  geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5, fiteach=TRUE) +
  scale_colour_brewer(palette="Set2") +
  facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
  geom_net(aes(colour= CitizenshipCountry), linewidth=0.5, fiteach=TRUE) +
  scale_colour_brewer(palette="Set2") +
  facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
  geom_net(aes(colour= CurrentEmploymentType), linewidth=0.5, fiteach=FALSE) +
  scale_colour_brewer(palette="Set2") +
  facet_wrap(~day, nrow=2) + theme(legend.position="bottom")
#'
## Les Miserables example
#'
data(lesmis)
lesmisnet <- merge(lesmis$edges, lesmis$vertices, by.x="from", by.y="label", all=TRUE)
p <- ggplot(data=lesmisnet, aes(from_id=from, to_id=to))
p + geom_net(layout="fruchtermanreingold")
p + geom_net(layout="fruchtermanreingold", label=TRUE, vjust=-0.5)
p + geom_net(layout="fruchtermanreingold", label=TRUE, vjust=-0.5, aes(linewidth=degree/5))
#'
## College Football Games in the Fall 2000 regular season
# Hello world!
# Source: http://www-personal.umich.edu/~mejn/netdata/
data(football)
ftnet <- merge(football$edges, football$vertices, by.x="from", by.y="label", all=TRUE)
p <- ggplot(data=ftnet, aes(from_id=from, to_id=to))
p + geom_net(aes(colour=value), linewidth=0.75, size=4.5, ecolour="grey80") +
  scale_colour_brewer("Conference", palette="Paired") + theme_net() +
  theme(legend.position="bottom")

end_vcontext()
