require(network)
require(RColorBrewer)

setwd("~/Documents/Code/R/ggnet/")

source("data.R")
source("functions.R")
source("ggnet.R")

# network
net = network(df)
mps = data.frame(Twitter = network.vertex.names(net))

# colours
mp.groups  = merge(mps, ids, by = "Twitter")$Groupe
mp.colors  = brewer.pal(9, "Set1")[c(3, 1, 9, 6, 8, 5, 2)]

# first example plot
ggnet(net, 
      weight = "degree", 
      quantize = TRUE,
      node.group = mp.groups, 
      node.color = mp.colors,
      names = c("Group", "Links")) + 
  theme(text = element_text(size = 16))
## ggsave(file = "example1.png")

# network density
ggnet(net, 
      size = 6, 
      segment.size = 0, 
      weight = "indegree", 
      legend = "none") + 
  geom_density2d()
## ggsave(file = "example2.png")

# rightwing parties
rightwing = ifelse(mp.groups == "NI", NA, mp.groups %in% c("UDI", "UMP"))
ggnet(net, 
      node.group = rightwing, 
      alpha = .25, 
      name = "Rightwing group")
## ggsave(file = "example3.png")

# followers of NKM
follows.nkm = ids$Twitter %in% who.follows(df, "nk_m")$followers
ggnet(net, 
      size = 6, 
      node.group = follows.nkm, 
      alpha = .25, 
      name = "Follows NKM",
      label = "nk_m", 
      color = "black")
## ggsave(file = "example4.png")
