# geomnet 0.2.0

This version of geomnet has been updated to account for changes in ggplot2 version 2.2.0.

## Brand New Features

- Introduction of `"adjmat"` and `"edgedf"` classes for newly added `fortify` methods. 
- `fortify` methods included for `"network"`, `"igraph"`, `"adjmat"`, and `"edgedf"` class objects to transform network objects to data frame objects that can be visualized using `geom_net()`.
- Legend icons are updated to better represent the network data. A micro-network is drawn instead of a single point or line.
- Added `plotly` functionality. Hovertext describes edges and provides node labels. Note that arrows are not currently supported in `plotly`, so directed networks will appear as undirected.
- New logical parameter `singletons` added for plotting all nodes in a network even if they have indegree and outdegree of zero. This is most likely to be useful when facetting. The default is `TRUE`, so all nodes are plotted in all panels. When `singletons = FALSE`, nodes are only plotted in the panel(s) in which they have at least one incoming or outgoing edge.   

## Small Usage Changes

- Plotting labels is slightly different. Use `labelon = TRUE` to plot labels with the node ids. Use `label` inside `aes()` to map a variable value to node label.
- The `selfies` argument for drawing self-loops on nodes has been changed to `selfloops`.


## Other file changes

- Vignette added with several side-by-side comparison examples of `geomnet` with `GGally::ggnet2()` and `ggnetwork` by [François Briatte](https://github.com/briatte/)

