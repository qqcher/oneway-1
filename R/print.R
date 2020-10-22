#' Print one-way ANOVA results
#'
#' print.oneway prints one-way ANOVA results
#'
#' @param x an object of class oneway.
#' @param ... additional arguments passed to the function.
#'
#' @return the input object is returned silently
#' @export
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
#' print(mileage)
print.oneway <- function(x, ...){

  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway")

  cat("\n\033[31m\033[1m\033[4mSummary Statistics\033[24m\033[22m\033[39m\n")
  print(x$summarystats, ...)
  cat("\n\033[31m\033[1m\033[4mANOVA\033[24m\033[22m\033[39m\n")
  print(summary.lm(x$anova))
}
