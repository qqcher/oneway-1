#' ANOVA assumptions
#'
#' assumptions() assesses ANOVA assumptions of normality of residuals,
#' and equality of group variances on the outcome variable
#'
#' @param fit an object of class 'oneway'.
#' @export
#'
#' @return two data frames, Q-Q plot and a boxplot
#' @examples
#' fit <- oneway(mpg~cyl, mtcars)
#' plot(fit)
#'
#' @import ggplot2 patchwork stats
assumptions <- function(fit){

  ## checking if the input is oneway class

  if(!inherits(fit, "oneway"))
    stop("Must be class 'oneway")

  ##Conduct "Shapiro-Wilk" Test for Normality.

  y <- fit$anova$model[[1]]
  x <- fit$anova$model[[2]]

  result_normality <- shapiro.test(y)[-3]
  result_normality$data.name <- as.character(fit$anova$terms[[2]])
  result_normality <- as.data.frame(result_normality)

  ##Create Q-Q plot of residuals

  text1 <- "Normal Q-Q Plot"
  text2 <- as.character(fit$anova$terms[[2]])
  text3 <- "by"
  text4 <- paste(text3, text2, sep = " ")


  #label <- paste(text1, text4, sep = "\n")

  label <- paste("Normal Q-Q Plot by", as.character(fit$anova$terms[[2]]), sep = " ")

  p <- ggplot(fit$anova$model, aes(sample = fit$anova$model[[1]]))
  qqplot <- p+
    stat_qq()+
    stat_qq_line()+
    ggtitle(label)

  ## Conduct "Flinger" Test for Equal Variance.


  data <- fit$anova$model

  fligner_result <- fligner.test(formula = y~x, data = data)
  fligner_result <- fligner_result[-4]
  fligner_result$data.name <- paste(fit$anova$terms[[2]],
                                    fit$anova$terms[[1]],
                                    fit$anova$terms[[3]],sep="")
  fligner_result <- as.data.frame(fligner_result)

  ## Create a Box-Plot
  g <- factor(x)

  med <- tapply(y, g, median)
  y <- y-med[g]

  fligner_plot <- ggplot(fit$anova$model, aes(x=g, y=y)) +
    geom_boxplot() +
    ggtitle("Homogeneity of Variance") +
    geom_hline(yintercept=0, linetype="dashed", color = "red") +
    xlab(as.character(fit$anova$terms[[3]])) +
    ylab(as.character(fit$anova$terms[[2]]))

  ## Printing the Assumption
  cat("\n\033[94m\033[1m\033[4m       Normality Testing       \033[24m\033[22m\033[39m\n")
  print(result_normality)
  cat("\n===============================\n")
  cat("\n\033[94m\033[1m\033[4m    Equal Variance Testing     \033[24m\033[22m\033[39m\n")
  print(fligner_result)

  qqplot + fligner_plot +
    plot_annotation(title = 'Testing of Assumptions')


}

