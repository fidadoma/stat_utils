#' Formats output from likelihood ratio test when comparing two lmer models
#'
#' @param av results of anova(lm1,lm2) function
#'
#' @return
#' @export
#'
format_lrt <- function(av) {
  sprintf("\\chisq^2(%d) = %.2f, p = %.3f", av$`Chi Df`[2], av$Chisq[2], av$`Pr(>Chisq)`[2])
}