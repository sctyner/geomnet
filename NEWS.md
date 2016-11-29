# geomnet 0.2.0

This version of geomnet has been updated to account for changes in ggplot2 version 2.2.0.

## Brand New Features

- Introduction of `"adjmat"` and `"edgedf"` classes for newly added `fortify` methods. 
- `fortify` methods included for `"network"`, `"igraph"`, `"adjmat"`, and `"edgedf"` class objects to transform network objects to data frame objects that can be visualized using `geom_net()`.
- Legend icons are updated to better represent the network data. A micro-network is drawn instead of a single point or line.
- Added `plotly` functionality. Hovertext on edges appears on the `to` node, and if a node has indegree greater than 1, only the last incoming edge's hovertext is shown.

## Small Usage Changes

- Plotting labels is slightly different. Use `labelon = TRUE` to plot labels with the node ids. Use `label` inside `aes()` to map a variable value to node label.
- The `selfies` argument for drawing self-loops on nodes has been changed to `selfloops`.


## Other file changes

- Vignette added with several side-by-side comparison examples of `geomnet` with `GGally::ggnet2()` and `ggnetwork` by [François Briatte](https://github.com/briatte/)

