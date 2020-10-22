#' Plot one-way ANOVA
#'
#' plot.oneway creates group comparisons for a one-way ANOVA.
#'
#' @param x an object of class oneway.
#' @param ... additional arguments passed to geom_boxplot function.
#'
#' @export
#' @import ggplot2
#'
#' @return ggplot2 graph
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' plot(mileage)
plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway")

  g <- as.character(x$anova$terms[[3]])
  y <- as.character(x$anova$terms[[2]])
  ggplot(x$anova$model, aes(x = factor(.data[[g]]), y = .data[[y]],
                            fill=factor(.data[[g]]))) +
    geom_boxplot(...) +
    labs(x=g) + theme(legend.position="none")
}
