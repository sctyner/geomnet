## ----setup, echo=FALSE---------------------------------------------------

knitr::opts_chunk$set(fig.width = 6,
                      fig.height = 4,
                      fig.align='center',
                      dev = "png")


## ----speed1_loadpkg, echo = TRUE, eval = FALSE, message=FALSE, warning = FALSE----
#  ## igraph (current: v1.0.1)
#  if (!require(igraph, quietly = TRUE)) {
#    install.package("igraph")
#  }
#  library(igraph)
#  
#  ## network (current: v1.13.0)
#  if (!require(network, quietly = TRUE)) {
#    install.package("network")
#  }
#  library(network)
#  
#  ## ggnet2
#  if (!require(GGally, quietly = TRUE)) {
#    install.packages("GGally")
#  }
#  library(GGally)
#  
#  ## geom_net
#  library(geomnet) # also currently requires dplyr
#  
#  ## ggnetwork
#  if (!require(ggnetwork, quietly = TRUE)) {
#    install.packages("ggnetwork")
#  }
#  library(ggnetwork)
#  
#  ## pre-loading
#  library(sna)
#  

## ---- echo = TRUE, message = FALSE---------------------------------------
## ggplot2
library(ggplot2)

## other packages
library(dplyr)
library(tidyr)

## load data 
data(protein, package = "geomnet")

## ----speed1_simcode, eval=FALSE, echo=TRUE-------------------------------
#  d = data.frame()
#  
#  if (!file.exists("runtimes-protein-100.csv")) {
#  
#    for (i in 1:100) {
#  
#      cat("Iteration", sprintf("%3.0f", i), "/ 100\n")
#  
#      n = as.matrix(protein$edges[, 1:2])
#      n = igraph::graph_from_edgelist(n, directed = FALSE)
#  
#      t1 = system.time({
#        plot(n, vertex.label = NA, layout = layout_randomly)
#      })[1]
#  
#      n = network(protein$edges[, 1:2], directed = FALSE)
#  
#      t2 = system.time({
#       plot(n, coord = gplot.layout.random(n, NULL))
#      })[1]
#  
#      t3 = system.time({
#        print(ggnet2(n, mode = "random"))
#      })[1]
#  
#      t4 = system.time({
#        print(ggplot(data = protein$edges, aes(from_id = from, to_id = to)) +
#                geom_net(layout = "random"))
#      })[1]
#  
#      t5 = system.time({
#        print(ggplot(ggnetwork(n, layout = "random"),
#                     aes(x, y, xend = xend, yend = yend)) +
#                geom_edges() +
#                geom_nodes())
#      })[1]
#  
#      d = rbind(d, data.frame(
#        iteration = i,
#        igraph = t1,
#        network = t2,
#        ggnet2 = t3,
#        geomnet = t4,
#        ggnetwork = t5,
#        row.names = NULL
#      ))
#  
#    }
#  
#    write.csv(d, file = "runtimes-protein-100.csv", row.names = FALSE)
#  
#  }

## ----speed1_cheatdata, echo=FALSE----------------------------------------
load("../inst/runtimes-protein.rda")

## ----speed1_viewdata, echo = TRUE, eval = FALSE--------------------------
#  link_to_data <- "runtimes-protein-100.csv"
#  g = read.csv(link_to_data, stringsAsFactors = FALSE) %>%
#        gather(`Visualization approach`, time, -iteration)

## ----speed1_plot, echo=TRUE----------------------------------------------
ggplot(g, aes(x = `Visualization approach`, y = time,
              fill = `Visualization approach`)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(y = "\nPlotting time (in seconds)", x = "Implementation\n") +
  guides(fill = FALSE) 

## ----speed_2code, eval = FALSE-------------------------------------------
#  # note: plot.network might be slower because it actually prints the plots, so to
#  # compare the methods, we need to evaluate the ggplot plots by printing them too
#  
#  # create 10 files per size
#  for (k in 0:9) {
#  
#    # the 10 different sizes
#    for (i in seq(250, 25, -25)) {
#  
#      f = paste0("runtimes-", sprintf("%04.0f", i), "-", k, ".csv")
#  
#      if (!file.exists(f)) {
#  
#        d = data.frame()
#  
#        # in each file store the runtimes of plotting 10 random graphs
#        # do this for each of the 5 methods
#        for (j in 1:10) {
#  
#          r = sna::rgraph(i, tprob = 0.2)
#  
#          cat("Network size", i,
#              "iteration", sprintf("%3.0f", 10 * k + j), "/ 100\n")
#  
#          n = igraph::graph_from_adjacency_matrix(r, mode = "undirected")
#  
#          t1 = system.time({
#            plot(n, vertex.label = NA)
#          })[1]
#  
#          n = network::network(r, directed = FALSE)
#  
#          t2 = system.time({
#            plot.network(n)
#          })[1]
#  
#          t3 = system.time({
#            print(ggnet2(n))
#          })[1]
#  
#          e = data.frame(sna::as.edgelist.sna(n))
#  
#          t4 = system.time({
#            print(ggplot(data = e) +
#                    geom_net(aes(from_id = X1, to_id = X2)))
#          })[1]
#  
#          t5 = system.time({
#            print(ggplot(ggnetwork(n),
#                         aes(x, y, xend = xend, yend = yend)) +
#                    geom_edges() +
#                    geom_nodes())
#          })[1]
#  
#          d = rbind(d, data.frame(
#            network_size = i,
#            iteration = 10 * k + j,
#            igraph = t1,
#            network = t2,
#            ggnet2 = t3,
#            geomnet = t4,
#            ggnetwork = t5,
#            row.names = NULL
#          ))
#  
#          dev.off()
#  
#        }
#  
#        write.csv(d, f, row.names = FALSE)
#  
#      }
#  
#    }

## ----speed2_code, echo = TRUE, eval=FALSE--------------------------------
#  # get all files containing runtime information and combine them into a single data frame.
#  g = list.files(pattern = "runtimes-(.*)csv$") %>%
#          lapply(read.csv, stringsAsFactors = FALSE) %>%
#          bind_rows %>%
#          gather(`Visualization approach`, time, -network_size, -iteration) %>%
#          group_by(network_size, `Visualization approach`) %>%
#          summarise(mean_time = mean(time),
#                    q05 = quantile(time, 0.05),
#                    q95 = quantile(time, 0.95))

## ----all-runtimes, eval=TRUE, echo=FALSE, fig.width = 8, fig.height = 8----
# link_to_full_comparison <- "https://raw.githubusercontent.com/sctyner/ggnet-paper/master/data/compare-all.csv"
# g <- readr::read_csv(link_to_full_comparison)
load("../inst/compare-all.rda")
qplot(data = g, network_size, time, colour = `Visualization approach`, alpha = I(0.1)) +
  facet_wrap(facets = ~setup, scales = "free_y") +
  geom_smooth(fill="white") +
  scale_colour_brewer("Implementation", palette = "Set1") +
  xlab("Network size (edge probability p = 0.2)") +
  ylab("Time (in seconds)") +
  theme_bw() +
  theme(legend.position = "bottom")

