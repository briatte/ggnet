#
# descriptive functions
#

setwd("~/Documents/Code/R/ggnet/")

source("data.R")

#
# who is followed by...
#
who.is.followed.by <- function(x) {
  f = net[which(net$Source == x), ]$Target
  print(as.character(f))
  print(table(ids$Groupe[which(ids$Twitter %in% f)]))
}
# examples
who.is.followed.by("JacquesBompard")
who.is.followed.by("Marion_M_Le_Pen")

#
# who follows...
#
who.follows <- function(x) {
  f = net[which(net$Target == x), ]$Source
  print(as.character(f))
  print(table(ids$Groupe[which(ids$Twitter %in% f)]))  
}
# examples
who.follows("nk_m")
who.follows("marclefur")

#
# within-group indegree
#
top.group.inlinks <- function(x) {
  x = ids$Twitter[which(ids$Groupe %in% x)]
  f = subset(net, Source %in% x & Target %in% x)
  f = data.frame(table(f$Target))
  return(head(f[order(f$Freq, decreasing = TRUE), ]))
}
# examples
top.group.inlinks("SRC")
lapply(levels(ids$Groupe), top.group.inlinks)

#
# within-group outdegree
#
top.group.outlinks <- function(x) {
  x = ids$Twitter[which(ids$Groupe %in% x)]
  f = subset(net, Source %in% x & Target %in% x)
  f = data.frame(table(f$Source))
  return(head(f[order(f$Freq, decreasing = TRUE), ]))
}
# examples
top.group.outlinks("Ecolo")
lapply(levels(ids$Groupe), top.group.outlinks)
