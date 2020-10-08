#' One Way Analysis of Variance
#'
#' oneway computes a one-way analysis of variance
#' and includes group level summary statistics.
#'
#' @param formula an object of class formula, relating the
#' dependent variable to the grouping variable.
#' @param data a data frame containing the variables in the model.
#'
#' @export
#' @return a list with 2 elements.
#'
#' @examples
#' mileage <- oneway(mpg ~ cyl, mtcars)
oneway <- function(formula, data){

  # delete missing data
  data <- na.omit(data)

  #anova
  fit <- lm(formula, data)

  # summary statistics
  stats <- aggregate(formula, data,
                     function(x) c(n=length(x),
                                   mean=mean(x),
                                   sd=sd(x)))

  # return results
  result <- list(anova = fit, summarystats = stats)
  class(result) <- "oneway"
  return(result)
}


