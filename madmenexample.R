#mad men example
data(madmen, package="gcookbook")
sex <- c("f", "m", "m", "f","m","f",rep("m",5),rep("f",15), "m", "m", "f","f","f","m","m","m","m",rep("f",8),"m","f")
mm.vert <- data.frame(Name = as.factor(unique(c(madmen$Name1,madmen$Name2))), Gender = as.factor(sex))

madmen <- list(edges = madmen, vertices = mm.vert)
#save(madmen, file = "~/Desktop/NetworksResearch/ggnet/CC/data/madmen.rda")

ggplot(data = madmen$edges, aes(from_id = Name1, to_id = Name2)) + geom_net(vertices = madmen$vertices) + 
  expand_limits(x = c(-10,10), y = c(-10,10)) + theme_net
