###############################################################################
# Get EZCDDM summary statistics: ##############################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function calculates summary statistics for circular and linear data:
# 1. MCA (Mean Circular Angle) - The mean direction of angular responses
# 2. MRT (Mean Response Time) - The average reaction time
# 3. VCA (Variance of Circular Angle) - Measure of angular dispersion (0-1)
# 4. VRT (Variance of Response Time) - Variance of reaction times
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

get_summaryStats <- function(angular_vector, rt_vector) {
    # Input vectors containing angular responses and response times
    ang <- angular_vector %% (2*pi)
    rt <- rt_vector
    N <- length(ang)  # Total number of observations/trials
    
    # Calculate Mean Circular Angle (MCA) using circular statistics
    MCA <- atan2(1/N*sum(sin(ang)), 1/N*sum(cos(ang)))
    
    # Calculate Variance of Circular Angle (VCA)
    # VCA ranges from 0 (perfect concentration) to 1 (uniform dispersion)
    VCA <- 1 - (1/N)*sqrt(sum(cos(ang))^2 + sum(sin(ang))^2)

    # Calculate Mean Response Time (MRT)
    MRT <- mean(rt)           
    # Calculate Variance of Response Time (VRT)
    VRT <- var(rt)
    
    # Return all summary statistics as a list
    output <- list("MCA" = MCA, "MRT" = MRT, "VCA" = VCA, "VRT" = VRT)
    return(output)
}

