# geomnet 0.2.0

This version of geomnet has been updated to account for changes in ggplot2 version 2.2.0.

## Brand New Features

- `fortify` methods included for `"network"`, `"igraph"`, `"matrix"`, and `"data.frame"` class objects to transform network objects to data frame objects that can be visualized using `geom_net()`.
- Legend icons are updated to better represent the network data. A micro-network is drawn instead of a single point or line.

## Small Usage Changes

- Plotting labels is slightly different. Use `labelon = TRUE` to plot labels with the node ids. Use `label` inside `aes()` to map a variable value to node label.
- The `selfies` argument for drawing self-loops on nodes has been changed to `selfloops`.


## Other file changes

- Vignette added with several side-by-side comparison examples of `geomnet` with `GGally::ggnet2()` and `ggnetwork` by [François Briatte](https://github.com/briatte/)

