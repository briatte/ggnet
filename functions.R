#
# group results
#
by.group <- function(f) {
  t = table(ids$Groupe[which(ids$Twitter %in% f)])
  print(t)
  print(round(t / sum(t), 2))
}

#
# who is followed by...
#
who.is.followed.by <- function(net, x) {
  f = net[which(net$Source == x), ]$Target
  by.group(f)
  return(list(node = x, follows = as.character(f)))
}
# examples
# x = who.is.followed.by(net, "JacquesBompard")
# str(x)
# y = who.is.followed.by(net, "Marion_M_Le_Pen")
# str(y)

#
# who follows...
#
who.follows <- function(net, x) {
  f = net[which(net$Target == x), ]$Source
  by.group(f)
  return(list(node = x, followers = as.character(f)))
}
# examples
# x = who.follows(net, "nk_m")
# str(x)
# y = who.follows(net, "marclefur")
# str(y)

#
# within-group indegree
#
top.group.inlinks <- function(net, x) {
  x = ids$Twitter[which(ids$Groupe %in% x)]
  f = subset(net, Source %in% x & Target %in% x)
  f = data.frame(table(f$Target))
  return(head(f[order(f$Freq, decreasing = TRUE), ]))
}
# examples
# top.group.inlinks(net, "SRC")
# lapply(levels(ids$Groupe), top.group.inlinks, df = df)

#
# within-group outdegree
#
top.group.outlinks <- function(net, x) {
  x = ids$Twitter[which(ids$Groupe %in% x)]
  f = subset(net, Source %in% x & Target %in% x)
  f = data.frame(table(f$Source))
  return(head(f[order(f$Freq, decreasing = TRUE), ]))
}
# examples
# top.group.outlinks(net, "Ecolo")
# lapply(levels(ids$Groupe), top.group.outlinks, df = df)
