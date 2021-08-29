#' Aperture formula
#'
#' Calculate the signal detected as a function of psi and beta for a circular
#' aperture, the function only applies where beta + psi < pi/2.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#' @param beta Half cone angle of the aperture (rad)
#' @return Detected intensity from the circular aperture
#' @export
intensityAnl <- function(psi, beta) 0.5*pi*cos(psi)*(1 - cos(2*beta) )


#' Aperture integral
#'
#' # Calculate the signal detected as a function of psi and beta for a circular
#' aperture, where the analytic formula cannot be applied. Use where
#' beta + psi >= pi/2.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#'            can be array type
#' @param beta Half cone angle of the aperture (rad), cannot be array type
#' @return Detected intensity from the circular aperture
#' @export
intensityInt <- function(psi, beta) {
    cosChi <- function(theta, phi, psi, beta) {
        dDotn <- cos(theta)*(sin(psi)*tan(theta)*cos(phi) + cos(psi))
        result <- if_else(dDotn < 0, 0, dDotn)
        return(result)
    }
    
    integrand <- function(theta, phi) sin(theta)*cosChi(theta, phi, psi, beta)
    result <- 2*quad2d(integrand, 0, beta, 0, pi)
    return(result)
}


#' Detected intensity in an aperture
#'
#' Calculate the signal detected as a function of psi and beta for a circular
#' aperture, chooses the analytic or integral model as appropriate.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#'            can be array type
#' @param beta Half cone angle of the aperture (rad), cannot be array type
#' @return Detected intensity from the circular aperture
#' @export
intensityCircular <- function(psi, beta) {
    if_else(psi + beta > pi/2,
            intensityInt(psi, beta),
            intensityAnl(psi, beta))
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


#' Elliptical aperture
#' 
#' Calcuates the signal detected for an elliptical aperture based on the two
#' principle half cone angles and the surface orientation.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#'            can be array type
#' @param beta_a Half cone angle 1, this is in the plane that the surface
#'               element is tilted in, (rad)
#' @param beta_b Half cone angle 2, this is perpendicular to the plane of the
#'               surface element tilt, (rad)
#' @return Detected intensity from the elliptical aperture
#' @export
intensityElliptical <- function(psi, beta_a, beta_b) {
    a <- tan(beta_a)
    b <- tan(beta_b)
    
    theta1 <- function(phi) atan(a*b/sqrt( a^2*(sin(phi))^2 + b^2*(cos(phi))^2 ))
    
    integrand1 <- function(phi) (theta1(phi) - sin(theta1(phi))*cos(theta1(phi)))*cos(phi) 
    integrand2 <- function(phi) 1 - (cos(theta1(phi)))^2
    
    0.5*sin(psi)*quad(integrand1, 0, 2*pi) + 0.5*cos(psi)*quad(integrand2, 0, 2*pi)
}


#--------------------------- Analysis functions -------------------------------#


# For a range of psis and fixed beta calculate the contrast to noise ratio
calc_CNR <- function(beta_value, psi_range) {
    psis_range <- seq(psi_range[1], psi_range[2], length.out = 200)
    beta_range <- vector(mode = "numeric", length = 200) + beta_value
    ints <- bicubic(x = psis, y = betas, z = mtx, 
                    x0 = psis_range, y0 = beta_range)
    return(c(grad_contrsat(ints$z, psi_range), mean(ints$z)))
}

# Calculate the CNR for a series of values of beta over a range of psis
calc_CNR_overBeta <- function(the_betas, psi_range) {
    CNR <- lapply(the_betas, calc_CNR, psi_range = psi_range)
    CNR <- matrix(unlist(CNR), nrow = 2)
    df_betas <- tibble(beta = the_betas, sig = CNR[1,], av = CNR[2,])
    return(df_betas)
}

# Convert angles to the lab frame to psi given the detection angle
psi_of_ang <- function(det_ang, ang_range) ang_range + det_ang






