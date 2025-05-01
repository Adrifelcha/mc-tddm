##############################################################
# Backward equations:
# Inverse of the forward equations
# Given the choice and rt data, estimate the model parameters
##############################################################
ezcddm_getParameters <- function(summaryStats) {
    # Get summary statistics
    MCA <- summaryStats$MCA
    VCA <- summaryStats$VCA
    VRT <- summaryStats$VRT
    MRT <- summaryStats$MRT
    
    # Compute parameters in order (Eq 4; Qarehdaghi et al., 2024)
    #############################################################

    # Step 1: Drift angle
    drift_angle <- MCA

    # Step 2: Intermediate variables
    R <- 1 - VCA                              # Mean resultant length
    k0 <- ( R* ( 2- (R^2) ) ) / (1 - (R^2) )  # Concentration parameter
    I1 <- besselI(k0, nu = 1)  # First kind of modified Bessel function, order 1
    I0 <- besselI(k0, nu = 0)  # First kind of modified Bessel function, order 0
    k1_num <- (I1/I0)-R
    k1_den <- 1 - (I1/I0)^2 - (I1/I0)/k0
    k1 <- k0 - (k1_num/k1_den)
    # Step 3: Drift length
    root <- 1/4    
    drift_length <- ((1/VRT)*((k1^2*R^2)+(2*k1*R)-(k1^2)))^root
    # Step 4: Bound
    bound <- k1/drift_length
    # Step 5: Non-decision time
    ndt <- MRT - ((bound/drift_length)*R)

    # Step 6: Return parameters
    output <- list("drift_length" = drift_length, "bound" = bound, "ndt" = ndt, "drift_angle" = drift_angle)
    return(output)
}


############################################################
# Forward equations:
# Predicted summary statistics for choice and rt data
# based on the model parameters
############################################################
ezcddm_MRT <- function(drift, boundary, tzero) {
    # Calculate av product
    av <- drift * boundary
    
    # Calculate ratio of modified Bessel functions
    I1 <- besselI(av, nu = 1)  # First kind, order 1
    I0 <- besselI(av, nu = 0)  # First kind, order 0
    
    # Calculate expected RT
    MRT <- tzero + (boundary/drift) * (I1/I0)
    
    return(MRT)
}


ezcddm_VRT <- function(drift, boundary) {  
    # Calculate av product
    av <- drift * boundary
    
    # Calculate modified Bessel functions
    I1 <- besselI(av, nu = 1)  # First kind, order 1
    I0 <- besselI(av, nu = 0)  # First kind, order 0
    
    # Calculate ratio once to avoid repetition
    ratio <- I1/I0
    

    a <- (boundary^2)/(drift^2)    

    # Calculate variance using the full equation:
    # ((b^2/v^2)(I1^2/I0^2)) + ((2b/v^3)(I1/I0)) - (b^2/v^2)
    VRT <- a * (ratio^2) + ((2*boundary)/(drift^3)) * ratio - a
    
    return(VRT)
}


ezcddm_VCA <- function(drift, boundary) {
    # Calculate av 
    av <- drift * boundary
    
    # Calculate ratio of modified Bessel functions
    I1 <- besselI(av, nu = 1)  # First kind, order 1
    I0 <- besselI(av, nu = 0)  # First kind, order 0
    
    # Calculate expected RT
    VCA <- 1 - (I1/I0)
    
    return(VCA)
}


