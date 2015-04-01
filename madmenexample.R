#mad men example
data(madmen, package="gcookbook")
sex <- c("f", "m", "m", "f","m","f",rep("m",5),rep("f",15), "m", "m", "f","f","f","m","m","m","m",rep("f",8),"m","f")
mm.vert <- data.frame(Name = as.factor(unique(c(madmen$Name1,madmen$Name2))), Gender = as.factor(sex))

madmen <- list(edges = madmen, vertices = mm.vert)
#save(madmen, file = "~/Desktop/NetworksResearch/ggnet/CC/data/madmen.rda")

ggplot(data = madmen$edges, aes(from_id = Name1, to_id = Name2)) + geom_net(vertices = madmen$vertices) + 
  expand_limits(x = c(-10,10), y = c(-10,10)) + theme_net

data(madmen2, package = "gcookbook")
head(madmen2)

label <- unique(c(madmen2$Name1,madmen2$Name2))
Gender <- c('m','f',rep('m',2), rep('f',3), 'm', 'f', 'm','f',rep('m',2), 'f', rep('m',2), 'f', rep('m',2), rep('f',9),
            rep('m',3), rep('f',4), 'm', 'f','m',rep('f',2), 'm', 'f','m','m','f','m',rep('f',6))

vert = data.frame(label,Gender)
head(vert)

mm.directed <- list(edges = madmen2, vertices = vert)
aes(vcolour=c( "#FF69B4", "#0099ff")[as.numeric(Gender)])

ggplot(data = mm.directed$edges, aes(from_id = Name1, to_id = Name2)) + 
  geom_net(vertices = mm.directed$vertices, layout= "random", l.param = list(dist = 'normal'),directed = T, vlabel = T, vsize = I(2.5), aes(vcolour=c( "#FF69B4", "#0099ff")[as.numeric(Gender)])) + 
  expand_limits(x = c(0,1.1), y = c(0,1)) + theme_net

save(mm.directed, file = "~/Dropbox/ggplot2/data/mm_directed.rda")
