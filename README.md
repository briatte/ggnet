# ggnet: simple network plots with ggplot2

A while ago, Moritz Marbach coded the [`plotg()`][mm] function to visualize networks with `ggplot2` in R.

Here's a slightly modified version that takes the following arguments:

		ggnet(net,                      # an object of class network
		  mode = "fruchtermanreingold", # placement algorithm
		  size = 12,                 # node size
		  alpha = .75,               # transparency
		  weight.method = "none",    # what to weight the nodes with: "degree", "indegree", "outdegree"
		  names = c("", ""),         # what to call the node color and node weight legends
		  node.group = NULL,         # what to color the nodes with
		  node.color = NULL,         # what colors to use for the node classes
		  node.alpha = NULL,         # transparency for nodes (inherits from alpha)
		  segment.alpha = NULL,      # transparency for links (inherits from alpha)
		  segment.color = "grey",    # default links are rgb(190, 190, 190)
		  segment.size  = .25,       # set to 0 to remove from plot
		  arrow.size = 0,            # set to 0 to remove from plot
		  label.nodes = FALSE,       # add vertex names in small print; can be a list of vertex names
		  quantize.weights = FALSE,  # break weights to quartiles
		  legend.position = "right") # set to "none" to remove from plot

[mm]: http://sumtxt.wordpress.com/2011/07/02/visualizing-networks-with-ggplot2-in-r/

The function needs an object of class `network` and automatically handles the conversion of objects of class `igraph` by calling the [`intergraph`][ig] package. It supports all placement algorithms available through the `sna` package.

[ig]: http://intergraph.r-forge.r-project.org/

Comments welcome!

# Example

![French MPs on Twitter](example1.png)

A plot of Twitter connexions between 339 French MPs currently in office, colored by parliamentary groups and quartile-weighted by degree. See [`functions.R`][fn] for network exploration routines. Data assembled by scraping a few web sources in May 2013 with the help of [Jonathan Chibois][jc] and Benjamin Ooghe-Tabanou from [Regards Citoyens][rc]. My [blog post at Polit'bistro][pb] has more details.

[bc]: http://coulmont.com/index.php?s=d%C3%A9put%C3%A9s
[jc]: http://laspic.hypotheses.org/
[rc]: http://www.regardscitoyens.org/
[eg]: http://freakonometrics.blog.free.fr/index.php?post/Twitter-deputes
[fn]: functions.R
[pb]: http://politbistro.hypotheses.org/1752

# Options

The `ggnet()` function returns a `ggplot` object in which nodes are represented by points that can be colored and/or weighted using proportional scaling. The network above can therefore be set to look like this when the segments are not drawn and the nodes are weighted by indegree and left uncolored:

![](example2.png)

		ggnet(net, size = 6, segment.size = 0, weight = "indegree", legend = "none") + 
		  geom_density2d()

The node colors are set through a group variable colored by a discrete palette. Node groups can be any vector containing as many items as there are nodes in the network. Hence, to verify that the dual structure shown above corresponds to the left-right party divide, we group nodes by a logical value and let the function select from the default `Set1` scheme to discriminate them:

![](example3.png)

		rightwing = ifelse(mp.groups == "NI", NA, mp.groups %in% c("UDI", "UMP"))
		ggnet(net, node.group = rightwing, alpha = .25, name = "Rightwing group")

Node colors and weights are optional and apply to all nodes. The function can also label all or a selection of nodes, identified by vertex names. See, for example, how party polarization is much less obvious when you look at a single individual's network (Nathalie Kosciusko-Morizet in this example, the rightwing candidate for the mayor of Paris in the next municipal election):

![](example4.png)

		nkm = list("nk_m", ids$Twitter %in% who.follows(df, "nk_m"))
		ggnet(net, size = 6, label = nkm[[1]], node.group = nkm[[2]], alpha = .25, name = "Follows NKM")

Inspired by [Baptiste Coulmont][bc] and [Ewen Gallic][eg].
