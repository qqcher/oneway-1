#' Summarize one-way ANOVA
#'
#' summary.oneway summarizes the results of the one-way ANOVA
#'
#' @param x an object of class oneway.
#' @export
#'
#' @return object of type anova and data.frame
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' summary(mileage)
summary.oneway <- function(x){
  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway")
  anova(x$anova)
}


