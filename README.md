---
title: "geom_net installation instructions"
author: "Sam Tyner"
---

Follow these simple steps to install the `geom_net` capabilities to `ggplot2`.

1. Sign into Github.
2. Go to [https://github.com/hadley/ggplot2](https://github.com/hadley/ggplot2) and clone the current version of `ggplot2`.
3. Go to [https://github.com/sctyner/ggnet](https://github.com/sctyner/ggnet) and download the `stat-net.r` and `geom-net.r` files.
4. Move `stat-net.r` and `geom-net.r` to the `R` folder in your cloned `ggplot2`.
5. Add `export(geom_net)` and `export(stat_net)` to the `ggplot2` NAMESPACE file.
6. `'stat-net.r'` and `'geom-net.r'` to the `ggplot2` DESCRIPTION file.
7. Open RStudio, and set your working directory to the location of the `ggplot2` clone.
8. `install.packages(c("network","sna"))`
9. `library(devtools)` (Or `install.packages("devtools")` then `library(devtools)`)
10. `install()` 
11. You can now use `geom_net`! If you want to run the examples, you also need to download the data folder from [https://github.com/sctyner/ggnet](https://github.com/sctyner/ggnet).
