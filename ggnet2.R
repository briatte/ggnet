ggnet2 <- function(
  net,                          # an object of class network
  mode             = "fruchtermanreingold", # placement algorithm
  layout.par       = NULL,      # placement options
  alpha            = 1,         # transparency
  color            = "grey75",
  palette          = NULL,
  color.palette    = palette,
  color.legend     = NA,
  shape            = 19,
  max_size         = 9,
  size             = 9,
  size.cut         = FALSE,
  size.min         = NA,
  size.max         = NA,
  size.legend      = NA,
  label            = FALSE,
  label.color      = "black",
  label.size       = max_size / 2,
  label.trim       = FALSE,
  node.alpha       = alpha,
  node.color       = color,
  node.label       = label,
  node.shape       = shape,
  node.size        = size,
  na.rm            = NA,
  edge.color       = "grey50",  # default links are rgb(190, 190, 190)
  edge.size        = .25,       # set to 0 to remove from plot
  edge.alpha       = 1,     # transparency for links (inherits from alpha)
  edge.lty         = "solid",
  edge.label       = NULL,      # label network at mid-edges
  edge.label.color = label.color,
  edge.label.size  = max_size / 2,
  edge.label.fill  = "white",
  arrow.size       = 0,         # set to 0 to remove from plot
  arrow.type       = "closed",  # set to 0 to remove from plot
  legend.size      = 9,
  legend.position  = "right",   # set to "none" to remove from plot
  ...                           # passed to geom_text for node labels
){

  # -- packages ----------------------------------------------------------------

  require(network      , quietly = TRUE) # network objects
  require(sna          , quietly = TRUE) # placement and centrality

  require(ggplot2      , quietly = TRUE) # grammar of graphics
  require(RColorBrewer , quietly = TRUE) # colors

  require(grid         , quietly = TRUE) # arrows
  require(scales       , quietly = TRUE) # sizing

  # -- conversion to network class ---------------------------------------------

  if (class(net) == "igraph" & require(intergraph, quietly = TRUE)) {
    net = intergraph::asNetwork(net)
  } else if (class("net") == "igraph") {
    stop("install the 'intergraph' package to use igraph objects with ggnet2")
  }

  if (!network::is.network(net)) {
    net = network::network(net)
  }

  if (!network::is.network(net)) {
    stop("could not coerce net to a network object")
  }

  # -- network functions -------------------------------------------------------

  get_v = get("%v%", envir = as.environment("package:network"))
  get_e = get("%e%", envir = as.environment("package:network"))

  set_mode = function(x, mode = network::get.network.attribute(x, "bipartite")) {
    c(rep("actor", mode), rep("event", network::network.size(x) - mode))
  }

  set_attr = function(x, value) {

    if (is.null(x) || is.na(x)) {
      stop(paste("incorrect", value, "value"))
    } else if (length(x) == n_nodes) {
      data$alpha = x
    } else if (length(x) > 1) {
      stop(paste("incorrect", value, "length"))
    } else if (x %in% v_attr) {
      data$alpha = get_v(net, x)
    } else if (x == "mode" & is_bip) {
      data$alpha = set_mode(net)
    } else {
      data$alpha = x
    }

  }

  # -- network structure -------------------------------------------------------

  n_nodes = network::network.size(net)
  n_edges = network::network.edgecount(net)

  is_bip = network::is.bipartite(net)
  is_dir = ifelse(network::is.directed(net), "digraph", "graph")

  if (!is.numeric(arrow.size) || arrow.size < 0) {
    stop("incorrect arrow.size value")
  } else if (arrow.size > 0 & is_dir == "graph") {
    warning("network is undirected; arrow.size ignored")
    arrow.size = 0
  }

  if (network::is.hyper(net)) {
    warning("ggnet2 does not know how to handle hyper graphs")
  }

  if (network::is.multiplex(net)) {
    warning("ggnet2 does not know how to handle multiplex graphs")
  }

  if (network::has.loops(net)) {
    warning("ggnet2 does not know how to handle self-loops")
  }

  # vertex attributes
  v_attr = network::list.vertex.attributes(net)

  # edge attributes
  e_attr = network::list.edge.attributes(net)

  # -- initialize dataset ------------------------------------------------------

  data = data.frame(label = get_v(net, "vertex.names"), stringsAsFactors = FALSE)

  data$alpha = set_attr(node.alpha , "node.alpha")
  data$color = set_attr(node.color , "node.color")
  data$shape = set_attr(node.shape , "node.shape")
  data$size  = set_attr(node.size  , "node.size")

  # -- node removal ------------------------------------------------------------

  if (!is.na(na.rm)) {

    if (length(na.rm) > 1) {

      stop("incorrect na.rm value")

    } else if (!na.rm %in% v_attr) {

      stop(paste("vertex attribute", na.rm, "was not found"))

    }

    x = which(is.na(get_v(net, na.rm)))
    message(paste("na.rm removed", length(x), "nodes out of", nrow(data)))

    if (length(x) > 0) {

      data = data[ -x, ]
      network::delete.vertices(net, x)

    }

  }

  # -- weight method -----------------------------------------------------------

  if (length(size) == 1 && size %in% c("indegree", "outdegree", "degree", "freeman")) {
    data$size = sna::degree(net, gmode = is_dir, cmode = ifelse(size == "degree", "freeman", size))
    size.legend = ifelse(is.na(size.legend), size, size.legend)
  }

  # -- weight thresholds -------------------------------------------------------

  x = ifelse(is.na(size.min), 0, size.min)

  if (length(x) > 1 || !is.numeric(x) || is.infinite(x)) {
    stop("incorrect size.min value")
  }

  if (x < 0) {
    stop("incorrect size.min value")
  }

  if (x > 0 && !is.numeric(data$size)) {

    warning("node.size is not numeric; ignoring size.min")
    x = 0

  }

  if (x > 0) {

    x = which(data$size < x)
    message(paste("size.min removed", length(x), "nodes out of", nrow(data)))

    if (length(x) > 0) {

      data = data[ -x, ]
      network::delete.vertices(net, x)

    }

  }

  if (!nrow(data)) {

    warning("size.min removed all nodes; nothing left to plot")
    return(invisible(NULL))

  }

  x = ifelse(is.na(size.max), 0, size.max)

  if (length(x) > 1 || !is.numeric(x) || is.infinite(x)) {
    stop("incorrect size.max value")
  }

  if (x < 0) {
    stop("incorrect size.max value")
  }

  if (x > 0 && !is.numeric(data$size)) {

    warning("node.size is not numeric; ignoring size.min")
    x = 0

  }

  if (x > 0) {

    x = which(data$size > x)
    message(paste("size.max removed", length(x), "nodes out of", nrow(data)))

    if (length(x) > 0) {

      data = data[ -x, ]
      network::delete.vertices(net, x)

    }

  }

  if (!nrow(data)) {

    warning("size.max removed all nodes; nothing left to plot")
    return(invisible(NULL))

  }

  # -- weight quantiles --------------------------------------------------------

  x = size.cut

  if (length(x) > 1 || is.na(x)) {
    stop("incorrect size.cut value")
  }

  # if TRUE, default to quartiles
  if (isTRUE(x)) {
    x = 4
  }

  # if FALSE, do not do anything
  if (!x) {
    x = 0
  }

  if (!is.numeric(x) || is.infinite(x)) {
    stop("incorrect size.cut value")
  }

  if (!is.integer(x)) {
    x = as.integer(x)
  }

  # cut weights to intervals
  if (x > 1) {
    x = unique(quantile(data$size, probs = seq(0, 1, by = 1 / x)))
  }

  if (length(x) > 1) {
    data$size = cut(data$size, unique(x), include.lowest = TRUE)
  } else if (x != 0) {
    warning("all nodes are identically weighted; size.cut ignored")
  }

  # -- color palettes ----------------------------------------------------------

  if (!is.null(color.palette)) {
    x = color.palette
  } else if (is.factor(data$color)) {
    x = levels(data$color)
  } else {
    x = unique(data$color)
  }

  # support ColorBrewer palettes
  if (length(x) == 1 && x %in% rownames(RColorBrewer::brewer.pal.info)) {

    data$color = factor(data$color)

    n_groups = length(levels(data$color))
    n_colors = RColorBrewer::brewer.pal.info[ x, "maxcolors" ]

    if (n_groups > n_colors) {
      stop(paste0("too many node groups (", n_groups, ") for ",
                  "color palette ", x, "(max: ", n_colors, ")"))
    } else if (n_groups < 3) {
      n_groups = 3
    }

    x = brewer.pal(n_groups, x)[ 1:length(levels(data$color)) ]
    names(x) = levels(data$color)

  }

  # check manual color palettes
  if (!is.null(names(x))) {

    y = unique(na.omit(data$color[ !data$color %in% names(x) ]))

    if (length(y) > 1) {
      stop(paste("no color for groups", paste0(y, collapse = ", ")))
    } else if (length(y) == 1) {
      stop(paste("no color for group", y))
    }

  }

  if (!is.numeric(x) && !all(is.color(x))) {

    data$color = factor(data$color)

    x = gray.colors(length(x))
    names(x) = levels(data$color)

  }

  color.palette = x

  # -- node labels -------------------------------------------------------------

  l = node.label

  if (isTRUE(l)) {
    l = data$label
  } else if (length(l) == n_nodes) {
    data$label = l
  } else if (length(l) == 1 && l %in% v_attr) {
    l = get_v(net, l)
  } else {
    l = ifelse(data$label %in% l, data$label, "")
  }

  # -- node labels: trimming ---------------------------------------------------

  x = label.trim

  if (is.logical(x) && !x) {
    x = 0
  }

  if (length(x) > 1 || !is.numeric(x)) {
    stop("incorrect label.trim value")
  }

  if (x > 0) {
    l[ nchar(l) > x ] = substr(l[ nchar(l) > x ], 1, x)
  }

  # -- node placement ----------------------------------------------------------

  if (length(mode) > 2) {
    stop("incorrect mode value")
  }

  if (length(mode) == 2) {

    if (!mode[1] %in% v_attr) {
      stop(paste("vertex attribute", mode[1], "was not found"))
    }

    if (!is.numeric(get_v(net, mode[1]))) {
      stop(paste("vertex attribute", mode[1], "is not numeric"))
    }

    if (!mode[2] %in% v_attr) {
      stop(paste("vertex attribute", mode[1], "was not found"))
    }

    if (!is.numeric(get_v(net, mode[2]))) {
      stop(paste("vertex attribute", mode[1], "is not numeric"))
    }

    xy = data.frame(x = get_v(net, mode[1]), y = get_v(net, mode[2]))

  } else {

    # find placement algorithm
    mode = paste0("gplot.layout.", mode)
    if (!exists(mode)) {
      stop(paste("unsupported placement method:", mode))
    }

    xy = network::as.matrix.network.adjacency(net)
    xy = do.call(mode, list(xy, layout.par))
    xy = data.frame(x = xy[, 1], y = xy[, 2])

  }

  data = cbind(data, xy)

  # -- edge colors -------------------------------------------------------------

  edges = network::as.matrix.network.edgelist(net)
  x = edge.color

  if (length(x) == 2 & x[1] == "color") {

    # edge colors [2]: color from source and target
    edge.color = ifelse(data$color[ edges[, 1]] == data$color[ edges[, 2]],
                        as.character(data$color[ edges[, 1]]), x[2])

    if (!is.null(names(color.palette))) {
      y = which(edge.color %in% names(color.palette))
      edge.color[ y ] = color.palette[ edge.color[ y ] ]
    }

    edge.color[ is.na(edge.color) ] = x[2]

  } else if (length(x) == 1 && x %in% e_attr) {

    # edge colors [3]: edge attribute
    edge.color = get_e(net, x)

  } else if (length(x) > 1 && length(x) != n_edges) {

    stop("incorrect edge.color length")

  }

  if (!all(is.color(edge.color)) & !all(is.numeric(edge.color))) {

    stop("incorrect edge.color value(s)")

  }

  # -- edge list ---------------------------------------------------------------

  edges = data.frame(xy[ edges[, 1], ], xy[ edges[, 2], ])
  names(edges) = c("X1", "Y1", "X2", "Y2")

  # get edge midpoints
  edges$midX = (edges$X1 + edges$X2) / 2
  edges$midY = (edges$Y1 + edges$Y2) / 2

  # -- edge labels -------------------------------------------------------------

  x = edge.label

  if (is.null(x)) {
    x = ""
  }

  if (length(x) == n_edges) {

    # custom edge labels [1]: vector of labels
    edges$label = x

  } else if (length(x) > 1) {

    stop("incorrect edge.label length")

  } else if (x %in% e_attr) {

    # custom edge labels [2]: vertex attribute
    edges$label = get_e(net, x)

  } else {

    # custom edge labels [3]: single value
    edges$label = x

  }

  # -- edge linetype -----------------------------------------------------------

  x = edge.lty

  if (length(x) > 1 && length(x) != n_edges) {

    stop("incorrect edge.lty length")

  } else if (x %in% e_attr) {

    # custom edge linetype: edge attribute
    edge.lty = get_e(net, x)

  }

  # -- edge size ---------------------------------------------------------------

  x = edge.size

  if (length(x) > 1 && length(x) != n_edges) {

    stop("incorrect edge.size length")

  } else if (length(x) > 1) {

    edge.size = x

  } else if (x %in% e_attr) {

    # custom edge size: edge attribute
    edge.size = get_e(net, x)

  }

  if (!is.numeric(edge.size)) {

    stop("incorrect edge.size value")

  }

  # -- plot edges --------------------------------------------------------------

  p = ggplot(data, aes(x = x, y = y)) +
    geom_segment(
      data = edges,
      aes(x = X1, y = Y1, xend = X2, yend = Y2),
      size   = edge.size,
      color  = edge.color,
      alpha  = edge.alpha,
      lty    = edge.lty,
      arrow  = arrow(
        type   = arrow.type,
        length = unit(arrow.size, "cm")
      )
    )

  if (!is.null(edge.label)) {

   p = p +
      geom_point(
        data = edges,
        aes(x = midX, y = midY),
        size   = edge.label.size,
        colour = edge.label.fill,
        alpha  = edge.alpha
      ) +
      geom_text(
        data = edges,
        aes(x = midX, y = midY, label = label),
        size   = edge.label.size,
        colour = edge.label.color
      )

  }

  # -- plot nodes --------------------------------------------------------------

  x = list()

  if (is.numeric(data$size) && length(unique(data$size)) == 1) {
    x = c(x, size = legend.size)
  }

  if (is.numeric(data$alpha) && length(unique(data$alpha)) == 1) {
    x = c(x, alpha = node.alpha)
  }

  if ((is.character(data$color) || is.numeric(data$color)) &&
      length(unique(data$color)) == 1) {
    x = c(x, colour = node.color) # must be English spelling
  }

  if (is.numeric(data$shape) && length(unique(data$shape)) == 1) {
    x = c(x, shape = node.shape)
  }

  p = p + geom_point(aes(alpha = factor(alpha), color = factor(color),
                         shape = factor(shape), size = factor(size)))

  if (is.numeric(data$alpha)) {
    v_alpha = unique(data$alpha)
    names(v_alpha) = unique(data$alpha)
    p = p + scale_alpha_manual("", values = v_alpha) + guides(alpha = FALSE)
  } else {
    p = p + scale_alpha_discrete(alpha, guide = guide_legend(override.aes = x))
  }

  if (!is.null(names(color.palette))) {

    p = p +
      scale_color_manual(ifelse(!is.na(color.legend), color.legend, node.color),
                         values = color.palette, guide = guide_legend(override.aes = x))

  } else if (is.numeric(data$color) | is.character(data$color)) {

    v_color = unique(data$color)
    names(v_color) = unique(data$color)
    p = p + scale_color_manual(values = v_color) + guides(color = FALSE)

  }

  if (is.numeric(data$shape)) {

    v_shape = unique(data$shape)
    names(v_shape) = unique(data$shape)
    p = p + scale_shape_manual("", values = v_shape, guide = guide_legend(override.aes = x))

    if (length(v_shape) == 1) {
      p = p + guides(shape = FALSE)
    }

  } else {
    p = p + scale_shape_discrete(shape, guide = guide_legend(override.aes = x))
  }

  if (is.numeric(data$size)) {

    v_size = rescale_max(unique(data$size))
    v_size = abs_area(max_size)(v_size)
    names(v_size) = unique(data$size)

    if (length(v_size) == 1) {
      v_size = size
      p = p + scale_size_manual(values = v_size) + guides(size = FALSE)
    } else {
      p = p + scale_size_manual(ifelse(is.na(size.legend), "", size.legend),
                                values = v_size,
                                guide = guide_legend(override.aes = x))
    }

  } else {
    p = p + scale_size_discrete(size,
                                range = c(max_size / length(unique(data$size)),
                                          max_size))
  }

  # -- plot node labels --------------------------------------------------------

  if (length(l) == n_nodes)
    p = p +
      geom_text(
        label = l,
        color = label.color,
        size  = label.size,
        ...
      )

  # finalize: remove grid, axes and scales
  p = p +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    coord_equal() +
    theme(
      panel.background = element_blank(),
      panel.grid       = element_blank(),
      axis.title       = element_blank(),
      legend.key       = element_blank(),
      legend.position  = legend.position,
      legend.text      = element_text(size = legend.size),
      legend.title     = element_text(size = legend.size)
    )

  return(p)

}
