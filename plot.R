require(network)
require(RColorBrewer)

setwd("~/Documents/Code/R/twitterpol/")

source("ggnet.R")

ids = read.csv("nodes.txt", sep = "\t")
names(ids)

net = read.csv("network.txt", sep = "\t")
names(net)

(net = network(net))

mps <- data.frame(Twitter = network.vertex.names(net))
mps.class <- merge(mps, ids, by = "Twitter")$Groupe
mps.color <- brewer.pal(9, "Set1")[c(3, 1, 9, 6, 8, 5, 2)]

# plot
ggnet(net, classes = mps.class, name = "Groupe", scheme = mps.color, 
      value = 12, weight = "sum", quartiles = T, labels = F)
