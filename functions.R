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
who.is.followed.by <- function(df, x) {
  f = df[which(df$Source == x), ]$Target
  by.group(f)
  return(list(node = x, follows = as.character(f)))
}
# examples
# x = who.is.followed.by(df, "JacquesBompard")
# str(x)
# y = who.is.followed.by(df, "Marion_M_Le_Pen")
# str(y)

#
# who follows...
#
who.follows <- function(df, x) {
  f = df[which(df$Target == x), ]$Source
  by.group(f)
  return(list(node = x, followers = as.character(f)))
}
# examples
# x = who.follows(df, "nk_m")
# str(x)
# y = who.follows(df, "marclefur")
# str(y)

#
# within-group indegree
#
top.group.inlinks <- function(df, x) {
  x = ids$Twitter[which(ids$Groupe %in% x)]
  f = subset(df, Source %in% x & Target %in% x)
  f = data.frame(table(f$Target))
  return(head(f[order(f$Freq, decreasing = TRUE), ]))
}
# examples
# top.group.inlinks(df, "SRC")
# lapply(levels(ids$Groupe), top.group.inlinks, df = df)

#
# within-group outdegree
#
top.group.outlinks <- function(df, x) {
  x = ids$Twitter[which(ids$Groupe %in% x)]
  f = subset(df, Source %in% x & Target %in% x)
  f = data.frame(table(f$Source))
  return(head(f[order(f$Freq, decreasing = TRUE), ]))
}
# examples
# top.group.outlinks(df, "Ecolo")
# lapply(levels(ids$Groupe), top.group.outlinks, df = df)
