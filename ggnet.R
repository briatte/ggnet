#' ggnet - Plot a network with ggplot2
#' 
#' Function for making a network plot starting from an object of class \code{network}, using ggplot2. The function builds on code by Moritz Marbach.
#' 
#' @export
#' @param net an object of class \code{igraph} or \code{network}. If the object is of class \code{igraph}, the \link{intergraph} package is used to convert it to class \code{network}.
#' @param mode a placement method from the list of modes provided in the \link{sna} package. Defaults to the Fruchterman-Reingold force-directed algorithm.
#' @param size Size of the network nodes. Defaults to 12. If the nodes are weighted, \code{size} is passed to \code{scale_size_area} in \link{ggplot2} to scale their areas proportionally.
#' @param alpha a level of transparency for all plot elements (nodes, vertices and arrows). Defaults to 0.75.
#' @param weight.method a weighting method for the nodes. Accepts "indegree", "outdegree" or "degree" (the default). Set to "none" or any unsupported character string to plot unweighted nodes.
#' @param node.group a vector of character strings to label the nodes with, of the same length and order as the vertex names. Factors are converted to strings prior to plotting.
#' @param node.color a vector of character strings to color the nodes with, holding as many colors as there are levels in the \code{node.group}. Tries to default to "Set1" if missing.
#' @param node.alpha Transparency of the nodes. Inherits from \code{alpha}.
#' @param segment.alpha Transparency of the vertex links. Inherits from \code{alpha}.
#' @param segment.color Color of the vertex links. Defaults to "grey".
#' @param segment.size Size of the vertex links. Defaults to 0.25.
#' @param arrow.size Size of the vertex arrows, assuming that the plot is directed. Defaults to 0. If set to \code{TRUE}, inherits from \code{segment.size}.
#' @param classes a vector of attributes to color the nodes with.
#' @param palette a ColorBrewer palette to be used for coloring the node groups. Defaults to \code{"Set1"} if the network has 2 to 9 classes, "#999999" otherwise.
#' @param label.nodes Label the nodes with their vertex attributes.
#' @param quantize.weights Break node weights to quartiles.
#' @param legend.position Location of the captions for node colors and weights. Accepts all positions supported by ggplot2 themes. Defaults to "right".
#' @param names A character vector of two elements to label the node groups and node weights with in the plot legend. Defaults to empty strings.
#' @seealso \code{\link{gplot}} in the \link{sna} package
#' @author Moritz Marbach \email{mmarbach@@mail.uni-mannheim.de}, François Briatte \email{f.briatte@@ed.ac.uk}
#' @examples
#' # Links between the Twitter accounts of French MPs in 2013.
#' require(downloader)
#' source_url("https://raw.github.com/briatte/ggnet/master/plot.R")

ggnet <- function(net, # an object of class network
  mode = "fruchtermanreingold", # placement algorithm
  size = 12,                # node size
  alpha = .75,              # transparency
  weight.method = "none",   # what to weight the nodes with: "degree", "indegree", "outdegree"
  names = c("", ""),        # what to call the node color and node weight legends
  node.group = NULL,        # what to color the nodes with
  node.color = NULL,        # what colors to use for the node classes
  node.alpha = NULL,        # transparency for nodes (inherits from alpha)
  segment.alpha = NULL,     # transparency for links (inherits from alpha)
  segment.color = "grey",   # default links are rgb(190, 190, 190)
  segment.size  = .25,      # set to 0 to remove from plot
  arrow.size = 0,           # set to 0 to remove from plot
  label.nodes = FALSE,      # add vertex names in small print; can be a list of vertex names
  quantize.weights = FALSE, # break weights to quartiles
  legend.position = "right")# set to "none" to remove from plot
  {
  require(ggplot2)       # plot
  require(grid)          # arrows
  require(intergraph)    # igraph conversion
  require(network)       # vertex attributes
  require(RColorBrewer)  # default color scheme
  require(sna)           # placement algorithm

  # support for igraph objects
  if(class(net) == "igraph") net = asNetwork(net)
  if(class(net) != "network") stop("net must be a network object of class 'network' or 'igraph'")
  
  # alpha default
  inherit <- function(x) ifelse(is.null(x), alpha, x)

  # arrow size
  if(isTRUE(arrow.size)) arrow.size = segment.size
  
  # node weights
  weight = weight.method
  quartiles = quantize.weights
  labels = label.nodes
    
  set.vertex.attribute(net, "elements", as.character(node.group))
  # get sociomatrix
  m <- as.matrix.network.adjacency(net)
  # get coordinates placement algorithm
  placement <- paste0("gplot.layout.", mode)
  if(!exists(placement)) stop("Unsupported placement method.")
  plotcord <- do.call(placement, list(m, NULL))
  plotcord <- data.frame(plotcord)
  colnames(plotcord) = c("X1", "X2")
  # get edgelist
  edglist <- as.matrix.network.edgelist(net)
  edges <- data.frame(plotcord[edglist[,1],], plotcord[edglist[,2],])
  plotcord$group <- as.factor(get.vertex.attribute(net, "elements"))
  
  # get weights
  degrees <- data.frame(id = network.vertex.names(net), 
                        indegree  = sapply(net$iel, length), 
                        outdegree = sapply(net$oel, length))
  degrees$degree <- with(degrees, indegree + outdegree)
  
  colnames(edges) <- c("X1", "Y1", "X2", "Y2")

  # set vertex names
  plotcord$id <- as.character(degrees$id)
  if(is.logical(labels)) {
    if(!labels) plotcord$id = ""
  } else plotcord$id[-which(plotcord$id %in% labels)] = ""

  # get vertice midpoints (not used later on)
  edges$midX  <- (edges$X1 + edges$X2) / 2
  edges$midY  <- (edges$Y1 + edges$Y2) / 2
  
  # plot the network
  pnet <- ggplot(plotcord, aes(X1, X2)) +
    # plot vertices (links)
    geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                 data = edges, 
                 size = segment.size, 
                 colour = segment.color, 
                 alpha = inherit(segment.alpha),
                 arrow = arrow(type = "closed", 
                               length = unit(arrow.size, "cm")))

  # null weighting
  if(!weight %in% c("degree", "indegree", "outdegree")) {
    if(weight != "none") warning("Unsupported weighting method; plotting unweigthed nodes.")
    pnet <- pnet + geom_point(data = plotcord, alpha = inherit(node.alpha), size = size)
  }
  else {
    plotcord$weight <- degrees[, which(names(degrees) == weight)]
    
    # show top weights
    cat(nrow(plotcord), "nodes, weighted by", weight, "\n\n")
    print(head(degrees[order(-degrees[weight]), ]))    

    # proportional scaling
    sizer <- scale_size_area(names[2], max_size = size)
    
    # quartiles
    if(quartiles) {
      plotcord$weight.label <- cut(plotcord$weight, 
                                   breaks = quantile(plotcord$weight),
                                   include.lowest = TRUE, ordered = TRUE)
      plotcord$weight <- as.integer(plotcord$weight.label)
      sizer <- scale_size_area(names[2], 
                               max_size = size, 
                               labels = levels(plotcord$weight.label))
    }
    
    # add to plot
    pnet <- pnet + geom_point(aes(size = weight),
                              data = plotcord, alpha = inherit(node.alpha)) + sizer
  }
  
  # default colors
  n = length(unique(node.group))
  if(length(node.color) != n) {
    warning("Node groups and node colors are of unequal length; using default colors.")
    if(n > 0 & n < 10) node.color = brewer.pal(9, "Set1")[1:n]
  }
    
  # color the nodes
  if(!is.null(node.group)) pnet <- pnet + 
    aes(colour = group) +
    scale_colour_manual(names[1], values = node.color,
                        guide = guide_legend(override.aes = list(size = sqrt(size)))) 

  # add text labels
  pnet <- pnet + geom_text(aes(label = id), color = "black")

  # finalize: remove grid, axes and scales
  pnet <- pnet +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) + 
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.key = element_rect(colour = "white"),
      legend.position = legend.position
    )

  return(pnet)
}