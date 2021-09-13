#' Hole conductance
#' 
#' Calculates the conductance of an elliptical hole with radii ra and rb.
#' 
#' @param ra One of the principle radii of the ellipse (cm)
#' @param rb The second princliple radii of the ellipse (cm) 
#' @return The conductance of the ellipse in litres per second
#' @export
hole_conductance <- function(ra, rb) {
    helium_factor <- 3.5
    helium_factor*11.6*pi*(ra*rb)
}

#' Aperture conductance
#' 
#' Calculates the pumping through a circular detector aperture.
#' 
#' @param beta The half cone angle (deg) of the detector aperture
#' @param working_dist The working distance from the sample to the aperture (mm)
#' @return The conductance of the aperture in litres per second
#' @export
apertureConductance <- function(beta, working_dist) {
    d <- working_dist/10
    r <- d*sin(beta*pi/180)
    hole_conductance(r, r)
}

#' Forwards cone conductance
#' 
#' Calculates the forwards conductance of a cone using the formula developed by
#' B. Mercier (doi.org/10.1116/1.2187996).
#' 
#' @param R0 Radius of the smaller opening (mm)
#' @param Rk Radius of the larger opening (mm)
#' @param L Length of the cone (mm)
#' @param alpha Angle of the sides of the cone (deg)
#' @return The conductance of the aperture in litres per second
#' @export
coneForwards <- function(R0, Rk, L, alpha, k) {
    # TODO: calculate k from the inputs
    R0 <- R0/1000 # Convert from mm to m
    Rk <- Rk/1000
    L <- L/1000
    kB <- 1.381e-23
    t <- 300 # Kelvin
    m_He <- 4*1.66e-27
    V_m <- sqrt(2*(3/2)*kB*t/m_He)
    Ca <- (4/3)*pi*V_m*(R0^2*Rk^2*k)/((R0 + Rk)*L)
    Ca <- Ca*1000 # Convert from m^3/s to litres/s
    return(Ca)
}

#' Backwards cone conductance
#'
#' Calculates the backwards conductance of a cone using the formula developed by
#' B. Mercier (doi.org/10.1116/1.2187996).
#' 
#' @param R0 Radius of the smaller opening (mm)
#' @param Rk Radius of the larger opening (mm)
#' @param L Length of the cone (mm)
#' @param alpha Angle of the sides of the cone (deg)
#' @return The conductance of the aperture in litres per second
#' @export
coneConductance <- function(R0, Rk, L, alpha, k) {
    # TODO: calculate k from the inputs
    Ca <- coneForwards(R0, Rk, L, alpha, k)
    gamma <- 1 + (16/3)*k*tan(alpha*pi/180)
    Cr <- Ca/gamma
    return(Cr)
}

#' Pumping effect
#' 
#' Calculates the effect of the pumping of the detector cone on the signal level
#' in the SHeM based on the conductance to the ionizer and the conductance back
#' out of the cone. 
#' 
#' @param C The conductance of the detector cone
#' @param C_det The condutance to the ionized (defaults to the Cambridge A-SHeM 
#'              A-SHeM detector)
#' @return The relative signal level due to the conductance, 1 would be if there
#'         were a 0% chance of back-conductance
#' @export 
pumping_effect <- function(C, C_det = 0.4) C_det/(C_det + C)
