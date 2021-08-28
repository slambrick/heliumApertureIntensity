#' Michelson contrast
#'
#' Calculates the Michelson contrast to noise for two intensity values.
#' 
#' @param I1 An intensity value
#' @param I2 A second intensity value
#' @return The Michelson contrast
#' @export
michelson <- function(I1, I2) {
    difference <- if_else(I1 > I2, I1 - I2, I2 - I1)
    CNR <- difference/(I1 + I2)^(3/2)
    return(CNR)
}

#' Difference to noise
#'
#' Calculates the difference to noise ratio (DNR) for two values.
#' 
#' @param I1 An intensity value
#' @param I2 A second intensity value
#' @return The difference to noise ratio
#' @export
diffNoise <- function(I1, I2) {
    difference <- if_else(I1 > I2, I1 - I2, I2 - I1)
    DNR <- difference/sqrt(I1 + I2)
    return(DNR)
}