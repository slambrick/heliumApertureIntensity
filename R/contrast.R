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


#' Detected intensity with radius
#' 
#' Calculates the signal detected as a function of an aperture radius and
#' distance from the sample as well as the surface orientation. Uses the
#' integral model where appropriate.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#'            can be array type
#' @param r Radius of the detector aperture (same units as d), cannot be array
#'          type
#' @param d Distance from the sample to the aperture (same units as r), cannot
#'          be array type
#' @return Detected intensity from the circular aperture
#' @export
intensityRadius <- function(psi, r, d) {
    tmp <- r/d
    beta <- atan(tmp)
    if_else(psi + beta > pi/2,
            intensityInt(psi, beta),
            pi*cos(psi)*(tmp^2/(tmp^2 + 1)))
}