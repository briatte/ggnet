# ggnet: network visualization with ggplot2

![](inst/demo.png)

This repository contains the latest versions of the `ggnet` and `ggnet2` functions, which allow to visualize networks as [`ggplot2`](http://ggplot2.org/) objects.

## INSTALL

`ggnet` and `ggnet2` are part of the [`GGally`](https://cran.r-project.org/web/packages/GGally/) package. Install it from CRAN:

```{r}
install.packages("GGally")
```

You can also install `ggnet` and `ggnet2` as a small standalone package:

```{r}
devtools::install_github("briatte/ggnet")
```

Note that you will need the latest version of [`ggplot2`](http://docs.ggplot2.org/current/) (2.0.0) for any of the functions to work properly.

## VIGNETTE

The `ggnet2` function is fully documented in [this vignette](https://briatte.github.io/ggnet/).

The data for one of the examples, a Twitter network of French Members of Parliament, is [included in this repository](inst/extdata), as is the [vignette source](vignettes).

## THANKS

- [Moritz Marbach](https://github.com/sumtxt) coded the [very first version of `ggnet`](http://sumtxt.wordpress.com/2011/07/02/visualizing-networks-with-ggplot2-in-r/)
- [Heike Hoffmann](https://github.com/heike) and [Ming-Yu Liu](https://github.com/ethen8181) provided code to improve the display of edge arrows
- [Pedro Jordano](https://github.com/pedroj) suggested adding support for [bipartite networks](https://github.com/pedroj/bipartite_plots)
- [Baptiste Coulmont](http://coulmont.com/index.php?s=d%C3%A9put%C3%A9s) and [Ewen Gallic](http://freakonometrics.blog.free.fr/index.php?post/Twitter-deputes) provided further inspiration
- [Barret Schloerke](https://github.com/schloerke) helps by maintaining the `GGally` package
