Network of Twitter follower ties between 339 French Members of Parliament. Data assembled in May 2013 with the help of [Jonathan Chibois](http://laspic.hypotheses.org/) and Benjamin Ooghe-Tabanou from [Regards Citoyens](http://www.regardscitoyens.org/).

The following code will create an object of class `network` from the two data files:

```R
require(network)

# node information
v = read.csv("nodes.tsv", sep = "\t")
names(v) = c("sex", "first_name", "last_name", "group", "constituency",
               "constituency_number", "committee", "screen_name")

# recode gender
v$sex = ifelse(v$sex == "M.", "M", "F")

# edge list
e = read.csv("network.tsv", sep = "\t")
names(e) = c("from", "to")

# network object
twitter = network(e, directed = TRUE)

# create vertex attributes
x = data.frame(screen_name = network.vertex.names(twitter))
for (i in names(v)) {
  y = merge(x, v, by = "screen_name", sort = FALSE)[, i ]
  set.vertex.attribute(twitter, i, as.character(y))
}

twitter
```

And here is some more code to create a vector of colors for the parliamentary groups:

```R
# parliamentary group colours
colors = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]
names(colors) = sort(unique(twitter %v% "group"))

colors
```

See [this blog post](http://politbistro.hypotheses.org/1752) (in French) for more details, and [this repository](https://github.com/briatte/elus) for similar data.
