#' Require packages
#'
#' This function is taken from the \code{GGally} package.
#'
#' @param pkgs vector of character values
#' @author Barret Schloerke
#' @keywords internal
require_pkgs = function(pkgs) {
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) {
      stop(paste("please install the package '", pkg, "'.  install.packages('", pkg, "') "))
    }
  }
}
