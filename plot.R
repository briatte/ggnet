require(network)
require(RColorBrewer)

setwd("~/Documents/Code/R/ggnet/")

source("data.R")
source("ggnet.R")

# network
net = network(net)
mps = data.frame(Twitter = network.vertex.names(net))

# settings
mp.groups  = merge(mps, ids, by = "Twitter")$Groupe
mp.colors = brewer.pal(9, "Set1")[c(3, 1, 9, 6, 8, 5, 2)]
large_font = theme(text = element_text(size = 16))

# plot
ggnet(net, size = 12, weight = "degree", weight.quartiles = TRUE,
      node.group = mp.groups, node.color = mp.colors,
      names = c("Group", "Links")) + large_font
