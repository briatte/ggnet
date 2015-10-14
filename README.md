# ggnet: network visualization with ggplot2

![](inst/demo.png)

This repository contains the latest versions of the `ggnet` and `ggnet2` functions, which allow to visualize networks as [`ggplot2`](http://ggplot2.org/) objects.

## INSTALL

`ggnet` and `ggnet2` are part of the __`GGally`__ package. Install it [from CRAN](https://cran.r-project.org/web/packages/GGally/):

```{r}
install.packages("GGally")
```

You can also use `ggnet` and `ggnet2` as standalone functions:

```{r}
source("https://raw.githubusercontent.com/briatte/ggnet/master/ggnet.R")
source("https://raw.githubusercontent.com/briatte/ggnet/master/ggnet2.R")
```

## VIGNETTE

The `ggnet2` function is fully documented in [this vignette](https://briatte.github.io/ggnet/).

The data for one of the examples, a Twitter network of French Members of Parliament, is [included in this repository](data), as is the [vignette source](vignette).

## THANKS

- [Moritz Marbach](https://github.com/sumtxt) coded the [very first version of `ggnet`](http://sumtxt.wordpress.com/2011/07/02/visualizing-networks-with-ggplot2-in-r/)
- [Heike Hoffmann](https://github.com/heike) and [Ming-Yu Liu](https://github.com/ethen8181) provided code to improve the display of edge arrows
- [Pedro Jordano](https://github.com/pedroj) suggested adding support for [bipartite networks](https://github.com/pedroj/bipartite_plots)
- [Baptiste Coulmont](http://coulmont.com/index.php?s=d%C3%A9put%C3%A9s) and [Ewen Gallic](http://freakonometrics.blog.free.fr/index.php?post/Twitter-deputes) provided further inspiration
- [Barret Schloerke](https://github.com/schloerke) helps by maintaining the `GGally` package
