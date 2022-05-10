#' @title Plate standardization
#' 
#' @description Standardize the data gathered for on a plate. This function is designed to apply any standardization on plate data. 
#' 
#' @param mat Matrix presenting the data gathered from a plate. See details.
#' @param FUN Function to be used to carry out the standardization.
#' 
#' @details 
#' 
#' The argument \code{mat} is a matrix that has the same organisation as the plate to be corrected. 
#' 
#' The function assumes that the standardization is performed on the whole data at once without reference to the matrix dimensions. In other words, the data is vectorized before performing the standardization. 
#' 
#' @return 
#' 
#' A matrix with the same dimensions as \code{mat} but with the data standardized.
#' 
#' @author F. Guillaume Blanchet
#' @keywords manip
#' @export
standardize <- function(mat, FUN){
  # Make sure FUN is a function
  FUN <- match.fun(FUN)
  
  # Convert mat to a vector
  vec <- as.vector(mat)
  
  # Standardize
  vecStand <- FUN(vec)
  
  # Convert back to a matrix
  matStand <- matrix(vecStand,
                     nrow = nrow(mat),
                     ncol = ncol(mat))
  
  # Return
  return(matStand)
}