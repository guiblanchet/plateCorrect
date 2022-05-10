#' @title Plate correction using a row-column standardization
#' 
#' @description Performs correction on a plate using a row-column standardization. Specifically, row means are calculated after having removed extreme values and each row is divided by this mean. Following, columns are standardized the same way.
#' 
#' @param mat Matrix presenting the data gathered from a plate. See details.
#' @param lower Numeric value presenting the lower fractional range of the data within each row (or column). This values needs to be range between 0 and 1. Default is 0.25. See details. 
#' @param upper Numeric value presenting the upper fractional range of the data within each row (or column). This values needs to be range between 0 and 1. Default is 0.75. See details.
#' 
#' @details 
#' 
#' The argument \code{mat} is a matrix that has the same organisation as the plate to be corrected. 
#' 
#' The argument \code{lower} needs to be always lower than the argument \code{upper} otherwise an error message will be sent.
#' 
#' @return 
#' 
#' A matrix with the same dimensions as \code{mat} but with the data corrected for edge effect problems.
#' 
#' @author F. Guillaume Blanchet
#' @keywords manip
#' @export
correctRC <- function(mat, lower = 0.25, upper = 0.75) {
	
  if(lower > upper){
    stop("'lower' needs to always be smaller than 'upper'")
  }
  
  # Center row
  rowMeans <- apply(mat, 1, RCBase, lower = lower, upper = upper)
  rowCent <- t(mat) %*% diag(1/rowMeans)
  
  # Center column
  colMeans <- apply(rowCent, 1, RCBase, lower = lower, upper = upper)
  colCent <- t(rowCent) %*% diag(1/colMeans)
  
  # Return result
  return(colCent)
}

# Basic function to make the correction
# This is meant to be an internal function
RCBase <- function(x, lower, upper) {
  mean(x[which(x > stats::quantile(x, probs = lower) & x < stats::quantile(x, probs = upper))])
}
