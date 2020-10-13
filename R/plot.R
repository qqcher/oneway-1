#' Plot one-way ANOVA
#'
#' plot.oneway creates group comparisons for a one-way ANOVA.
#'
#' @param x an object jof class oneway.
#' @param ... additional arguments passed to boxplot function.
#'
#' @export
#'
#' @return NULL
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' plot(mileage)
plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway")
  boxplot(x$anova$terms, x$anova$model, ...)
}
