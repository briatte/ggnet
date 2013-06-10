#' ggnet - Plot a network with ggplot2
#' 
#' Function for making a network plot starting from an object of class \code{network}, using ggplot2. The function builds on code by Moritz Marbach.
#' 
#' @export
#' @param net an object of class \code{network}. See the \link{intergraph} package for conversion between objects of class \code{network} and \code{igraph}.
#' @param mode a placement method from the list of modes provided in the \link{sna} package. Defaults to the Fruchterman-Reingold force-directed algorithm.
#' @param size Size of the network nodes. Defaults to 12. If the nodes are weighted, \code{size} is passed to \code{scale_size_area} in \link{ggplot2} to scale their areas proportionally.
#' @param alpha a level of transparency for all plot elements (nodes, vertices and arrows). Defaults to 0.75 so that arrows remain visible below nodes.
#' @param segment.size Size of vertex lines. Defaults to 0.75.
#' @param arrow.size Size of the vertex arrows. Defaults to 0.25.
#' @param weight a weighting method for the nodes. Accepts "indegree", "outdegree" or "degree" (the default). Set to "none" or any unsupported character string to plot unweighted nodes.
#' @param weight.quartiles Create quartiles out of node weights.
#' @param classes a vector of attributes to color the nodes with.
#' @param palette a ColorBrewer palette to be used for coloring the node groups. Defaults to \code{"Set1"} if the network has 2 to 9 classes, "#999999" otherwise.
#' @param label.nodes Label the nodes with their vertex attributes.
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
                  size = 12,          # node size
                  alpha = .75,        # transparency
                  segment.size = .25, # segment width
                  arrow.size = .25,   # arrow width
                  weight.method = "degree",  # also accepts indegree, outdegree or none
                  weight.quartiles = FALSE,  # break weights to quartiles
                  node.group = NULL,       # what to color the nodes with
                  node.color = NULL,       # color classes for the nodes
                  label.nodes = FALSE,        # add vertex names in small print
                  names = c("", ""),  # captions for legends
                  legend.position = "right")
  {
  require(ggplot2)       # plot
  require(grid)          # arrows
  require(network)       # vertex attributes
  require(RColorBrewer)  # default color scheme
  require(sna)           # placement algorithm

  # node weights
  weight = weight.method
  quartiles = weight.quartiles
  labels = label.nodes
  
  # node colors
  nodes.classes = as.character(node.group)
  nodes.palette = node.color
  
  # captions
  nodes.caption = names[1]
  weights.name  = names[2]

  # default colors
#   if(is.null(palette)) {
#     n = length(unique(nodes.classes))
#     if(n == 1) palette = "grey"
#     if(n > 1)
#     palette = brewer.pal(, "Set1")
#   }
  
  set.vertex.attribute(net, "elements", nodes.classes)
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
  plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
  
  # get weights
  degrees <- data.frame(id = network.vertex.names(net), 
                        indegree  = sapply(net$iel, length), 
                        outdegree = sapply(net$oel, length))
  degrees$degree <- with(degrees, indegree + outdegree)
  
  colnames(edges) <- c("X1", "Y1", "X2", "Y2")

  # get vertice midpoints (not used later on)
  edges$midX  <- (edges$X1 + edges$X2) / 2
  edges$midY  <- (edges$Y1 + edges$Y2) / 2
  
  # plot the network
  pnet <- ggplot()  + 
    # plot vertices (links)
    geom_segment(aes(x = X1, y = Y1, xend = X2, yend = Y2), 
                 data = edges, size = segment.size, colour = "grey", alpha = alpha,
                 arrow = arrow(type = "closed", 
                               length = unit(arrow.size, "cm")))

  # null weighting
  if(!weight %in% c("degree", "indegree", "outdegree")) {
    if(weight != "none") warning("Unsupported weighting method, so plotting unweigthed nodes.")
    pnet <- pnet + geom_point(aes(X1, X2, colour = elements),
                              data = plotcord, alpha = alpha, size = size)
  }
  else {
    plotcord$weight <- degrees[, which(names(degrees) == weight)]
    
    # show top weights
    cat(nrow(plotcord), "nodes, weighted by", weight, "\n\n")
    print(head(degrees[order(-degrees[weight]), ]))    

    # proportional scaling
    sizer <- scale_size_area(weights.name, max_size = size)
    
    # quartiles
    if(quartiles) {
      plotcord$weight.label <- cut(plotcord$weight, 
                                   breaks = quantile(plotcord$weight),
                                   include.lowest = TRUE, ordered = TRUE)
      plotcord$weight <- as.integer(plotcord$weight.label)
      sizer <- scale_size_area(weights.name, 
                               max_size = size, 
                               labels = levels(plotcord$weight.label))
    }
    
    # add to plot
    pnet <- pnet + geom_point(aes(X1, X2, colour = elements, size = weight),
                              data = plotcord, alpha = alpha) + sizer
  }
  
  # color the nodes
  pnet <- pnet + scale_colour_manual(nodes.caption, values = nodes.palette,
                        guide = guide_legend(override.aes=list(size = sqrt(size)))) 

  pnet <- pnet +
    # remove grid, axes and scales
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) + 
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.key = element_rect(colour = "white"),
      legend.position = legend.position
    )
  
  if(labels) pnet <- pnet + geom_text(aes(X1, X2),
                                      data  = plotcord,
                                      label = degrees$id,
                                      size  = 4) # should be readable

  return(pnet)
}