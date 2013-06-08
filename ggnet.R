## Adapted from Moritz Marbach:
## http://sumtxt.wordpress.com/2011/07/02/visualizing-networks-with-ggplot2-in-r/

ggnet <- function(net, # an object of class network
                  value = 12,        # base node size
                  weight = "sum",    # also accepts indegree and outdegree
                  quartiles = FALSE, # break weights to quartiles
                  classes = NULL,    # what to color the nodes with
                  name = "",         # what to call the node color legend
                  scheme = NULL,     # color classes for the nodes
                  labels = FALSE,    # add vertex names in small print
                  legend.position = "right", arrow.size = 0.25) # cosmetics
  {
  require(ggplot2)       # plot
  require(grid)          # arrows
  require(network)       # vertex attributes
  require(RColorBrewer)  # default color scheme
  require(sna)           # placement algorithm
  
  set.vertex.attribute(net, "elements", as.character(classes))
  # get sociomatrix
  m <- as.matrix.network.adjacency(net)
  # get coordinates from Fruchterman-Reingold force-directed placement algorithm
  plotcord <- data.frame(gplot.layout.fruchtermanreingold(m, NULL)) 
  colnames(plotcord) = c("X1", "X2")
  # get edgelist
  edglist <- as.matrix.network.edgelist(net)
  edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
  plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
  
  # weights
  degrees <- data.frame(id = network.vertex.names(net), 
                        indegree = sapply(net$iel, length), 
                        outdegree = sapply(net$oel, length))
  degrees$sum <- with(degrees, indegree + outdegree)
  plotcord$weight <- degrees[, which(names(degrees) == weight)]
  sizer <- scale_size_area("Connexions", max_size = value)
  
  cat(nrow(plotcord), "nodes, weighted by", weight, ":\n")
  print(head(degrees[order(-degrees[weight]), ]))
  
  # quartiles
  if(quartiles) {
    plotcord$weight.label <- cut(plotcord$weight, 
                                 breaks = quantile(plotcord$weight),
                                 include.lowest = TRUE, ordered = TRUE)
    plotcord$weight <- as.integer(plotcord$weight.label)
    sizer <- scale_size_area("Connexions", 
                             max_size = value, 
                             labels = levels(plotcord$weight.label))
  }
  
  # labels
  labelizer <- NULL
  if(labels) labelizer <- geom_text(aes(X1, X2), 
                                    data = plotcord, 
                                    label = degrees$id, 
                                    size = 4)
  
  colnames(edges) <- c("X1","Y1","X2","Y2")

  # get vertice midpoints (not used later on)
  edges$midX  <- (edges$X1 + edges$X2) / 2
  edges$midY  <- (edges$Y1 + edges$Y2) / 2
  
  # default color scheme
  if(is.null(scheme)) scheme = brewer.pal(length(unique(classes)), "Set1")
  
  # plot the network
  pnet <- ggplot()  + 
    # plot vertices (links)
    geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                 data = edges, size = 0.25, colour = "grey", alpha = .75,
                 arrow = arrow(type = "closed", 
                               length = unit(arrow.size, "cm"))) +
    # plot nodes (points)
    geom_point(aes(X1, X2, colour = elements, size = weight),
               data = plotcord, alpha = 0.75) +
    # color the nodes
    scale_colour_manual(name, 
                        values = scheme, 
                        guide = guide_legend(override.aes = list(size = sqrt(value)))) +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labelizer + sizer +
    # discard default grid + titles in ggplot2
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.key = element_rect(colour = "white"),
      legend.position = legend.position
    )
  return(print(pnet))
}