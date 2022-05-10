#' @title Plate correction using autoregressive model
#' 
#' @description Performs correction on a plate using an autoregressive model as a basis. In short, a autoregressive model is used where the eight closest neighbours of each colonie are used to make the correction (a queen pattern). It is highly recommended to standardize the data before performing the analysis. The function \code{\link{standardize}} was designed to perform plate standardization. 
#' 
#' @param mat Matrix presenting the data gathered from a plate. See details.
#' @param diagWeight The weight used for the colonize located diagonally of the focal colony. Default is one over the square-root of 2. 
#' 
#' @details 
#' 
#' The argument \code{mat} is a matrix that has the same organisation as the plate to be corrected. 
#' 
#' In this function it is assume that closest neighbour of each colonie (up-down and left-right) are have the strongest influence and as such have a weight of 1, in this respect, the values given \code{diagWeight} have to be smaller or equal to 1. Conversely, a negative weight does not make sense either.
#' 
#' @return 
#' 
#' A matrix with the same dimensions as \code{mat} but with the data corrected for edge effect problems.
#' 
#' @author F. Guillaume Blanchet
#' @keywords manip
#' @export
correctAuto <- function(mat, diagWeight = 1/sqrt(2)){
  
  # Matrix size
  nR <- nrow(mat)
  nC <- ncol(mat)
  
  # Build samples coordinates
  xy <- as.matrix(expand.grid(1:nR, 1:nC))
  
  # Construct listw object linking the samples
  nbObj <- spdep::cell2nb(nC, nR, type = "queen")
  distLink <- spdep::nbdists(nbObj,xy)
  link <- spdep::nb2mat(nbObj, glist = distLink, style = "B")
#  link[which(link > 1, arr.ind = TRUE)] <- diagWeight
  link <- spdep::mat2listw(link)
  
  # Build model
  val <- as.vector(t(mat))
  model <- spatialreg::errorsarlm(val ~ 1, listw = link)
  
  # Extract residuals
  residError <- stats::residuals(model)
  
  # Return residuals
  res <- matrix(residError, nrow = nR, ncol = nC, byrow = TRUE)
  
  return(res)
}