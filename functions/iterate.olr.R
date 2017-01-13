#' TODO: Consider making this flexible so data is optional
#' Currently forces all three to be added:
#' data$response
#' data$predictor
#' But should have flexibility to add vectors themselves
#' response (a vector)
#' predictor (a vector)

#' Run ordinal logistic regression on each level of a factor
#' 
#' Performs ordinal logistic regression using the \code{polr} function from the 
#' MASS package. Iterates over all levels of the factor in \code{predictor}, using
#' each level as the reference to which other levels are compared.
#' 
#' @param response index or name of response variable; should refer to an ordered factor
#' @param predictor index or name of predictor factor
#' @param data data frame to use
#' 
#' @return a list of the following elements
#' \describe{
#'   \item{p.value}{symmetric matrix of p-values}
#'   \item{t.value}{symmetric values of t values}
#'   \item{adj.p}{numeric of the Bonferroni-adjusted p-value}
#' }
#' 
#' @examples
#' \dontrun{
#'   topic.olr <- iterate.olr(response = "preference", predictor = "topic", data = topics.wide)
#' }
iterate.olr <- function(response, predictor, data) {
  if (!require(package = "MASS")){
    stop("Could not run iterate.olr, package 'MASS' not found.")
  }

  orig.levels <- levels(data[, predictor])
  values.matrix <- matrix(NA, length(orig.levels), length(orig.levels))
  rownames(values.matrix) <- colnames(values.matrix) <- orig.levels
  olr.results <- list("p.values" = values.matrix, "t.values" = values.matrix)
  
  for (i in 1:length(orig.levels)) {
    data[, predictor] <- relevel(x = data[, predictor], ref = orig.levels[i])
    olr <- polr(formula = data[, response] ~ data[, predictor], Hess = TRUE)
    olr.coeff <- coef(summary(olr))
    olr.p <- pnorm(abs(olr.coeff[, "t value"]), lower.tail = FALSE) * 2
    olr.coeff <- cbind(olr.coeff, "p.value" = olr.p)
    olr.results[["p.values"]][levels(data[, predictor])[-1], i] <- olr.coeff[1:length(x = orig.levels) - 1, "p.value"]
    olr.results[["t.values"]][levels(data[, predictor])[-1], i] <- olr.coeff[1:length(x = orig.levels) - 1, "t value"]
  }
  
  adj.p <- 0.05/((length(orig.levels) * (length(orig.levels) - 1)) / 2)
  olr.results$adj.p <- adj.p
  return(olr.results)  
}
