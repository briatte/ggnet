#
# who is followed by...
#
who.is.followed.by <- function(df, x) {
  f = df[which(df$Source == x), ]$Target
  print(as.character(f))
  print(table(ids$Groupe[which(ids$Twitter %in% f)]))
}
# examples
who.is.followed.by(df, "JacquesBompard")
who.is.followed.by(df, "Marion_M_Le_Pen")

#
# who follows...
#
who.follows <- function(df, x) {
  f = df[which(df$Target == x), ]$Source
  print(table(ids$Groupe[which(ids$Twitter %in% f)]))  
  return(as.character(f))
}
# examples
who.follows(df, "nk_m")
who.follows(df, "marclefur")

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
top.group.inlinks(df, "SRC")
lapply(levels(ids$Groupe), top.group.inlinks, df = df)

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
top.group.outlinks(df, "Ecolo")
lapply(levels(ids$Groupe), top.group.outlinks, df = df)
