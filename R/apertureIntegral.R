#' Aperture formula
#'
#' Calculate the signal detected as a function of psi and beta for a circular
#' aperture, the function only applies where beta + psi < pi/2.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#' @param beta Half cone angle of the aperture (rad)
#' @return Detected intensity from the circular aperture
intensityAnl <- function(psi, beta) 0.25*pi*cos(psi)*(1 - cos(2*beta) )


#' Aperture integral
#'
#' # Calculate the signal detected as a function of psi and beta for a circular
#' aperture, where the analytic formula cannot be applied. Use where
#' beta + psi >= pi/2.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (rad)
#' @param beta Half cone angle of the aperture (rad)
#' @return Detected intensity from the circular aperture
intensityInt <- function(psi, beta) {
    cosChi <- function(theta, phi, psi, beta) {
        dDotn <- cos(theta)*(sin(psi)*tan(theta)*cos(phi) + cos(psi))
        result <- if_else(dDotn < 0, 0, dDotn)
        return(result)
    }
    
    integrand <- function(theta, phi) sin(theta)*cosChi(theta, phi, psi, beta)
    result <- quad2d(integrand, 0, beta, 0, pi)
    return(result)
}

#' Detected intensity in an aperture
#'
#' Calculate the signal detected as a function of psi and beta for a circular
#' aperture, chooses the analytic or integral model as appropriate.
#' 
#' @param psi Angle from the surface normal to the centre of the aperture (deg)
#' @param beta Half cone angle of the aperture (deg)
#' @return Detected intensity from the circular aperture
#' @export
intensity <- function(psi, beta) {
    psi <- psi*pi/180
    beta <- beta*pi/180
    result <- if_else(psi + beta > pi/2, intensityInt(psi, beta), intensityAnl(psi, beta))
    return(result)
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






