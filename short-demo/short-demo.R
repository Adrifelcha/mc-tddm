###############################################################################
###############################################################################
#####     A short demonstration of the monte carlo approximation to a
#####           categorical implementation of the CDDM
###############################################################################
###############################################################################
#~~~~~~~~~~ Load libraries and source files ~~~~~~~~~~~~~~~~
###############################################################################
# R libraries needed
library(here)
# Load main loading function
source(here("src/set_up", "load_functions.R"))
load_allCustomFunctions(show=FALSE) # Call the rest of the functions

###############################################################################
#~~~~~~~~~~ Run the short demo ~~~~~~~~~~~~~~~~
###############################################################################
z <- run_short_MCroutine(n = 1000, nCat = 10, nIter = 1000, show=TRUE, 
                    data_path = here("output", "toyData.RData"), forceRun = TRUE,
                    par = list(drift = 2.0, angle = 0.2, boundary = 2, tzero = 0.6),
                    fig1_path = here("short-demo", "fig1_choice_data.png"),
                    fig2_path = here("short-demo", "fig2_recovery.png"))