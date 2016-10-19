## ----load_packages, echo=TRUE--------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2) # needs to be version ≥ 2.1.0
library(scales)

## ggnet2
# if (!require(ggnet, quietly = TRUE)) {
#   devtools::install_github("briatte/ggnet")
# }
# library(ggnet)
if (!require(GGally, quietly = TRUE)) {
  devtools::install_github("ggobi/ggally")
}

## geom_net
if (!require(geomnet, quietly = TRUE)) {
  devtools::install_github("sctyner/geomnet")
}

## ggnetwork
if (!require(ggnetwork, quietly = TRUE) ||
    packageVersion("ggnetwork") < "0.2.1") {
  devtools::install_github("briatte/ggnetwork")
}

## pre-load
library(network)
library(sna)
library(GGally)
library(geomnet)
library(ggnetwork)

## ----madmen_geom_net, fig.width=7, fig.height=7.5------------------------
# make data accessible 
data(madmen, package = "geomnet")

# code for geom_net
# data step: merge edges and nodes by the "from" column
MMnet <- merge(madmen$edges, madmen$vertices,
               by.x = "Name1", by.y = "label", all = TRUE)
# create plot
set.seed(10052016)
ggplot(data = MMnet, aes(from_id = Name1, to_id = Name2)) +
  geom_net(aes(colour = Gender), layout = "kamadakawai", 
           size = 2, label = TRUE, vjust = -0.6, ecolour = "grey60",
           directed =FALSE, fontsize = 3, ealpha = 0.5) +
  scale_colour_manual(values = c("#FF69B4", "#0099ff")) +
  xlim(c(-0.05, 1.05)) +
  theme_net() +
  theme(legend.position = "bottom")

## ----madmen_ggnet2, eval=FALSE, echo=TRUE, fig.cap="The code required to generate the Mad Men example using the `ggnet2` function in the `GGally` package."----
#   library(GGally)
#   library(network)
#   # make the data available
#   data(madmen, package = 'geomnet')
#   # data step for both ggnet2 and ggnetwork
#   # create undirected network
#   mm.net <- network(madmen$edges[, 1:2], directed = FALSE)
#   # create node attribute (gender)
#   rownames(madmen$vertices) <- madmen$vertices$label
#   mm.net %v% "gender" <- as.character(
#     madmen$vertices[ network.vertex.names(mm.net), "Gender"]
#   )
#   # gender color palette
#   mm.col <- c("female" = "#ff69b4", "male" = "#0099ff")
#   # create plot for ggnet2
#   set.seed(10052016)
#   ggnet2(mm.net, color = mm.col[ mm.net %v% "gender" ],
#          label = TRUE, label.color = mm.col[ mm.net %v% "gender" ],
#          size = 2, vjust = -0.6, mode = "kamadakawai", label.size = 3)

## ----madmen_ggnetwork, echo=TRUE, eval=FALSE, fig.cap="The code required to generate the Mad Men example using the `ggnetwork` package."----
#  # create plot for ggnetwork. uses same data created for ggnet2 function
#   library(ggnetwork)
#   set.seed(10052016)
#   ggplot(data = ggnetwork(mm.net, layout = "kamadakawai"),
#          aes(x, y, xend = xend, yend = yend)) +
#     geom_edges(color = "grey50") +
#     geom_nodes(aes(colour = gender), size = 2) +
#     geom_nodetext(aes(colour = gender, label = vertex.names),
#                   size = 3, vjust = -0.6) +
#     scale_colour_manual(values = mm.col) +
#     xlim(c(-0.05, 1.05)) +
#     theme_blank() +
#     theme(legend.position = "bottom")

## ----blood_ggnet2,  echo=TRUE, fig.width=6, fig.height=6, warning=FALSE, fig.cap="`ggnet` implementation"----
# make data accessible
data(blood, package = "geomnet")

# plot with ggnet2 (Figure 2a)
set.seed(12252016)
ggnet2(network(blood$edges[, 1:2], directed=TRUE), 
       mode = "circle", size = 15, label = TRUE, 
       arrow.size = 10, arrow.gap = 0.05, vjust = 0.5,
       node.color = "darkred", label.color = "grey80")

## ----blood_geom_net, echo=TRUE, fig.width=6, fig.height=6, fig.cap="`geom_net` implementation"----
# plot with geomnet (Figure 2b)
set.seed(12252016)
ggplot(data = blood$edges, aes(from_id = from, to_id = to)) +
  geom_net(colour = "darkred", layout = "circle", label = TRUE, size = 15,
           directed = TRUE, vjust = 0.5, labelcolour = "grey80",
           arrowsize = 1.5, linewidth = 0.5, arrowgap = 0.05,
           selfloops = TRUE, ecolour = "grey40") + 
  theme_net() 

## ----blood_ggnetwork, echo=TRUE, fig.width=6, fig.height=6, fig.cap="`ggnetwork` implementation"----
# plot with ggnetwork (Figure 2c)
set.seed(12252016)
ggplot(ggnetwork(network(blood$edges[, 1:2]),
                 layout = "circle", arrow.gap = 0.05),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(color = "grey50",
             arrow = arrow(length = unit(10, "pt"), type = "closed")) +
  geom_nodes(size = 15, color = "darkred") +
  geom_nodetext(aes(label = vertex.names), color = "grey80") +
  theme_blank()

## ----email_ggnet2, size="footnotesize", opts.label="codefig", echo=TRUE----
# make data accessible
data(email, package = 'geomnet')

# create node attribute data
em.cet <- as.character(
  email$nodes$CurrentEmploymentType)
names(em.cet) = email$nodes$label

# remove the emails sent to all employees
edges <- subset(email$edges, nrecipients < 54)
# create network
em.net <- edges[, c("From", "to") ]
em.net <- network(em.net, directed = TRUE)
# create employee type node attribute
em.net %v% "curr_empl_type" <-
  em.cet[ network.vertex.names(em.net) ]
set.seed(10312016)
ggnet2(em.net, color = "curr_empl_type",
       size = 4, palette = "Set1",
       arrow.size = 5, arrow.gap = 0.02,
       edge.alpha = 0.25, mode = "fruchtermanreingold",
       edge.color = c("color", "grey50"),
       color.legend = "Employment Type") +
  theme(legend.position = "bottom")

## ----email_geom_net, size="footnotesize", opts.label="codefig", echo=TRUE, out.width='\\textwidth'----
# data step for the geomnet plot
emailnet <- merge(
  subset(email$edges, nrecipients < 54),
  email$nodes,
  by.x = "From", by.y = "label", all = TRUE)

set.seed(10312016)
ggplot(data = emailnet,
       aes(from_id = From, to_id = to)) +
  geom_net(layout = "fruchtermanreingold",
    aes(colour = CurrentEmploymentType,
        group = CurrentEmploymentType,
        linewidth = 3 * (...samegroup.. / 8 + .125)),
    ealpha = 0.25,
    size = 4, curvature = 0.05,
    directed = TRUE, arrowsize = 0.5) +
  scale_colour_brewer("Employment Type", palette = "Set1") +
  theme_net() +
  theme(legend.position = "bottom")

## ----email_ggnetwork, size="footnotesize", opts.label="codefig", echo=TRUE----
# use em.net created in ggnet2step
set.seed(10312016)
ggplot(ggnetwork(em.net, arrow.gap = 0.02,layout = "fruchtermanreingold"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(color = curr_empl_type),
    alpha = 0.25,
    arrow = arrow(length = unit(5, "pt"),
                  type = "closed"),
    curvature = 0.05) +
  geom_nodes(aes(color = curr_empl_type),
             size = 4) +
  scale_color_brewer("Employment Type",
                     palette = "Set1") +
  theme_blank() +
  theme(legend.position = "bottom")

## ----email_facet_ggnet2, fig.height=4, fig.width=8, echo=TRUE, out.width='\\textwidth'----
# ggnet2 code for the email network facetted by day as shown in fig.4a

# data preparation
em.day <- subset(email$edges, nrecipients < 54)[, c("From", "to", "day") ]
# create one element in a list per day
em.day <- lapply(unique(em.day$day),
                 function(x) subset(em.day, day == x)[, 1:2 ])
# create list of networks
em.day <- lapply(em.day, network, directed = TRUE)
# create node (employee type) and network (day) attributes for each element in list
for (i in 1:length(em.day)) {
  em.day[[ i ]] %v% "curr_empl_type" <-
    em.cet[ network.vertex.names(em.day[[ i ]]) ]
  em.day[[ i ]] %n% "day" <- unique(email$edges$day)[ i ]
}

# plot ggnet2
g <- list(length(em.day))
set.seed(7042016)
# plot each element in list
for (i in 1:length(em.day)) {
  g[[ i ]] <- ggnet2(em.day[[ i ]], size = 2, color = "curr_empl_type",
                     palette = "Set1", arrow.size = 0, arrow.gap = 0.01,
                     edge.alpha = 0.1, legend.position = "none", 
                     mode = "kamadakawai") +
    ggtitle(paste("Day", em.day[[ i ]] %n% "day")) +
    theme(panel.border = element_rect(color = "grey50", fill = NA),
          aspect.ratio = 1)
}
gridExtra::grid.arrange(grobs = g, nrow = 2)

## ----email_facet_geom_net, fig.height=4.5, fig.width=8, echo=TRUE--------
# geomnet code for the  email network facetted by day as shown in fig.4b

# data step: making sure that there is one entry for each person on each day
employee <- data.frame(expand.grid(label = unique(email$nodes$label),
                                   day = unique(email$edges$day)))
employee <- merge(employee, email$nodes, by = "label")
emailnet <- merge(subset(email$edges, nrecipients < 54), employee,
                  by.x = c("From", "day"), by.y = c("label", "day"),
                  all = TRUE)

# creating the plot
set.seed(7042016)
ggplot(data = emailnet, aes(from_id = From, to_id = to)) +
  geom_net(layout = "kamadakawai",
    aes(colour = CurrentEmploymentType,
        group = CurrentEmploymentType,
        linewidth = 2 * (...samegroup.. / 8 + .125)),
        fiteach = TRUE, ealpha = 0.5, size = 1.5) +
  scale_colour_brewer("Employment Type", palette = "Set1") +
  theme_net() +
  facet_wrap(~day, nrow = 2, labeller = "label_both") +
  theme(legend.position = "bottom",
        panel.border = element_rect(fill = NA, colour = "grey60"),
        plot.margin = unit(c(0, 0, 0, 0), "mm"))

## ----email_facet_ggnetwork, fig.height=4.5, fig.width=8,echo=TRUE--------
# ggnetwork code for the  email network facetted by day as shown in fig.4c

# create the network and aesthetics
edges <- subset(email$edges, nrecipients < 54)
edges <- edges[, c("From", "to", "day") ]
em.net <- network(edges[, 1:2])
# assign edge attributes (day)
set.edge.attribute(em.net, "day", edges[, 3])
# assign node attributes (employee type)
em.net %v% "curr_empl_type" <- em.cet[ network.vertex.names(em.net) ]

# create the plot
set.seed(7042016)
ggplot(ggnetwork(em.net, arrow.gap = 0.02, by = "day", 
                 layout = "kamadakawai"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(color = curr_empl_type),
    alpha = 0.25,
    arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_nodes(aes(color = curr_empl_type), size = 1.5) +
  scale_color_brewer("Employment Type", palette = "Set1") +
  facet_wrap(~day, nrow = 2, labeller = "label_both") +
  theme_facet(legend.position = "bottom")

## ----theme_ggnet2, echo=TRUE---------------------------------------------
# make data accessible
data(theme_elements, package = "geomnet")

# create network object
te.net <- network(theme_elements$edges)
# assign node attribut (size based on node degree)
te.net %v% "size" <-
  sqrt(10 * (sna::degree(te.net) + 1))
set.seed(3272016)
ggnet2(te.net, label = TRUE, color = "white", label.size = "size",
       mode = "fruchtermanreingold", layout.exp = 0.15)

## ----theme_geom_net, echo=TRUE, fig.width=7, fig.height = 7--------------
# data step: merge nodes and edges and
# introduce a degree-out variable
TEnet <- merge(
  theme_elements$edges,
  theme_elements$vertices,
  by.x = "parent", by.y = "name", all = TRUE)
TEnet <- TEnet %>%
  group_by(parent) %>%
  mutate(degree = sqrt(10 * n() + 1))

# create plot:
set.seed(3272016)
ggplot(data = TEnet,
       aes(from_id = parent, to_id = child)) +
  geom_net(layout = "fruchtermanreingold",
    aes(fontsize = degree), directed = TRUE,
    label = TRUE, size = 1, labelcolour = 'black',
    ecolour = "grey70", arrowsize = 0.5,
    linewidth = 0.5, repel = TRUE) +
  theme_net() +
  xlim(c(-0.05, 1.05))

## ----theme_ggnetwork, echo=TRUE------------------------------------------
set.seed(3272016)
# use network created in ggnet2 data step
ggplot(ggnetwork(te.net, layout = "fruchtermanreingold"),
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes(size = 12, color = "white") +
  geom_nodetext(
    aes(size = size, label = vertex.names)) +
  scale_size_continuous(range = c(4, 8)) +
  guides(size = FALSE) +
  theme_blank()

## ----football_ggnet2,echo=TRUE-------------------------------------------
#make data accessible
data(football, package = 'geomnet')
rownames(football$vertices) <-
  football$vertices$label
# create network 
fb.net <- network(football$edges[, 1:2],
                  directed = TRUE)
# create node attribute (what conference is team in?)
fb.net %v% "conf" <-
  football$vertices[
    network.vertex.names(fb.net), "value"
    ]
# create edge attribute (between teams in same conference?)
set.edge.attribute(
  fb.net, "same.conf",
  football$edges$same.conf)
set.seed(5232011)
ggnet2(fb.net, mode = "fruchtermanreingold",
       color = "conf",  palette = "Paired",
       color.legend = "Conference",
       edge.color = c("color", "grey75"))

## ----football_geom_net, fig.height=10, echo=TRUE-------------------------
# data step: merge vertices and edges
ftnet <- merge(
  football$edges, football$vertices,
  by.x = "from", by.y = "label", all = TRUE)

# create new label variable for viewing independent schools
ftnet$schools <- ifelse(
  ftnet$value == "Independents", ftnet$from, "")

# create data plot
set.seed(5232011)
ggplot(data = ftnet,
       aes(from_id = from, to_id = to)) +
  geom_net(layout = 'fruchtermanreingold',
    aes(colour = value, group = value,
        linetype = factor(same.conf != 1),
        label = schools),
    linewidth = 0.5,
    size = 5, vjust = -0.75, alpha = 0.3) +
  theme_net() +
  theme(legend.position = "bottom") +
  scale_colour_brewer("Conference", palette = "Paired")  +
  guides(linetype = FALSE)

## ----football_ggnetwork, echo=TRUE---------------------------------------
# use network from ggnet2 step
set.seed(5232011)
ggplot(
  ggnetwork(
    fb.net, 
    layout = "fruchtermanreingold"),
  aes(x, y, xend = xend, yend = yend)) +
  geom_edges(
    aes(linetype = as.factor(same.conf)),
    color = "grey50") +
  geom_nodes(aes(color = conf), size = 4) +
  scale_color_brewer("Conference",
                     palette = "Paired") +
  scale_linetype_manual(values = c(2,1)) +
  guides(linetype = FALSE) +
  theme_blank()

## ----davis_data_1, echo=TRUE---------------------------------------------
# access the data and rename it for convenience
library(tnet)
data(tnet)
elist <- data.frame(Davis.Southern.women.2mode)
names(elist) <- c("Lady", "Event")
detach(package:tnet)
detach(package:igraph)
head(elist)
elist$Lady <- paste("L", elist$Lady, sep="")
elist$Event <- paste("E", elist$Event, sep="")
davis <- elist
names(davis) <- c("from", "to")
davis <- rbind(davis, data.frame(from=davis$to, to=davis$from))
davis$type <- factor(c(rep("Lady", nrow(elist)), rep("Event", nrow(elist))))

## ----davis_ggnet2, echo=TRUE, warning=FALSE------------------------------
# Southern women network in ggnet2
# create affiliation matrix
bip = xtabs(~Event+Lady, data=elist)

# weighted bipartite network
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")

# detect and color the mode
set.seed(8262013)
ggnet2(bip, color = "mode", palette = "Set2", 
       shape = "mode", mode = "kamadakawai",
       size = 15, label = TRUE) +
  theme(legend.position="bottom")

## ----davis_geom_net, echo=TRUE-------------------------------------------
# Southern women network in geomnet
# change labelcolour
davis$lcolour <- 
  c("white", "black")[as.numeric(davis$type)]

set.seed(8262013)
ggplot(data = davis) + 
  geom_net(layout = "kamadakawai",
    aes(from_id = from, to_id = to, 
        colour = type, shape = type), 
    size = 15, label = TRUE, ealpha = 0.25,
    vjust = 0.5, hjust = 0.5,
    labelcolour = davis$lcolour) +
  theme_net() + 
  scale_colour_brewer("Type of node", palette = "Set2") +
  scale_shape("Type of node") +
  theme(legend.position = "bottom")

## ----davis_ggnetwork, echo=TRUE, warning=FALSE---------------------------
# Southern women network in ggnetwork. Use data from ggnet2 step
# assign vertex attributes (Node type and label)
set.vertex.attribute(bip, "mode", 
  c(rep("event", 14), rep("woman", 18)))
set.seed(8262013)
ggplot(data = ggnetwork(bip, layout = "kamadakawai"),
       aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(colour = "grey80") +
  geom_nodes(aes(colour = mode, shape = mode), size = 15) +
  geom_nodetext(aes(label = vertex.names)) +
  scale_colour_brewer(palette = "Set2") +
  theme_blank() + 
  theme(legend.position = "bottom") 

## ----bikes_geom_net, echo=TRUE, warning=FALSE, fig.keep='all'------------
# make data accessible
data(bikes, package = 'geomnet')
# data step for geomnet
tripnet <- merge(bikes$trips, bikes$stations, by.x = "Start.station",
                 by.y = "name", all = TRUE)
# create variable to identify Metro Stations
tripnet$Metro = FALSE
idx <- grep("Metro", tripnet$Start.station)
tripnet$Metro[idx] <- TRUE

# plot the bike sharing network shown in Figure 7b
set.seed(1232016)
ggplot(aes(from_id = Start.station, to_id = End.station), data = tripnet) +
  geom_net(aes(linewidth = n / 15, colour = Metro),
           label = TRUE, repel = TRUE) +
  theme_net() +
  xlim(c(-0.1, 1.1)) +
  scale_colour_manual("Metro Station", values = c("grey40", "darkorange")) +
  theme(legend.position = "bottom")

## ----bikes_prepare, echo = TRUE------------------------------------------
# data preparation for ggnet2 and ggnetwork
bikes.net <- network(bikes$trips[, 1:2 ], directed = FALSE)
# create edge attribute (number of trips)
network::set.edge.attribute(bikes.net, "n", bikes$trips[, 3 ] / 15)
# create vertex attribute for Metro Station
bikes.net %v% "station" <-  grepl("Metro", network.vertex.names(bikes.net))
bikes.net %v% "station" <-  1 + as.integer(bikes.net %v% "station")
rownames(bikes$stations) <- bikes$stations$name
# create node attributes (coordinates)
bikes.net %v% "lon" <-
  bikes$stations[ network.vertex.names(bikes.net), "long" ]
bikes.net %v% "lat" <-
  bikes$stations[ network.vertex.names(bikes.net), "lat" ]
bikes.col <- c("grey40", "darkorange")

## ----bikes_ggnet2, echo = TRUE, fig.keep='all'---------------------------
# Non-geographic placement
set.seed(1232016)
ggnet2(bikes.net, mode = "fruchtermanreingold", size = 4, label = TRUE,
       vjust = -0.5, edge.size = "n", layout.exp = 1.1,
       color = bikes.col[ bikes.net %v% "station" ],
       label.color = bikes.col[ bikes.net %v% "station" ])

## ----bikes_ggnetwork, echo = TRUE, fig.keep='all'------------------------
# Non-geographic placement. Use data from ggnet2 step.
set.seed(1232016)
ggplot(data = ggnetwork(bikes.net, layout = "fruchtermanreingold"),
         aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(size = n), color = "grey40") +
  geom_nodes(aes(color = factor(station)), size = 4) +
  geom_nodetext(aes(label = vertex.names, color = factor(station)),
                vjust = -0.5) +
  scale_size_continuous("Trips", breaks = c(2, 4, 6), labels = c(30, 60, 90)) +
  scale_colour_manual("Metro station", labels = c("FALSE", "TRUE"),
                      values = c("grey40", "darkorange")) +
  theme_blank() +
  theme(legend.position = "bottom", legend.box = "horizontal")

## ----geographic_common, echo=TRUE----------------------------------------
library(ggmap)
metro_map <- get_map(location = c(left = -77.22257, bottom = 39.05721, 
                                  right = -77.11271, top = 39.14247))

## ----geographic_geomnet, echo=TRUE---------------------------------------
# geomnet: overlay bike sharing network on geographic map
  ggmap(metro_map) + 
  geom_net(data = tripnet, layout = NULL, label = TRUE,
           vjust = -0.5, ealpha = 0.5,
           aes(from_id = Start.station,
               to_id = End.station,
               x = long, y = lat,
               linewidth = n / 15,
               colour = Metro)) +
  scale_colour_manual("Metro Station", values = c("grey40", "darkorange")) +
  theme_net() %+replace% theme(aspect.ratio=NULL) +
  coord_map() + 
  theme(legend.position = "bottom")

## ----compare, echo = FALSE, eval = FALSE, fig.height = 4, fig.width = 8----
#  g = data(runtimes_protein_100, package = "geomnet") %>%
#        gather(`Visualization approach`, time, -iteration)
#  
#  ggplot(g, aes(x = reorder(`Visualization approach`, time, median),
#                y = time,
#                color = `Visualization approach`,
#                fill = `Visualization approach`)) +
#    geom_boxplot(alpha = 0.6) +
#    scale_fill_brewer(palette = "Set1") +
#    scale_colour_brewer(palette = "Set1") +
#    labs(x = "Visualisation approach\n", y = "\nAverage plotting time (seconds)") +
#    ylim(c(0, NA)) +
#    coord_flip() +
#    theme_bw() + # HH: gridlines help with comparisons. significantly.
#    theme(legend.position = "none") +
#    theme(axis.text = element_text(size = rel(1)))

