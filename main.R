###############################################################################
###############################################################################
#####      A script containing the main function to simulate data from the 
#####               CDDM with random walk drift
###############################################################################
###############################################################################

############################################################
#~~~~~~~~~~ Load libraries and source files ~~~~~~~~~~~~~~~~
############################################################
# Load libraries
library(here)

# Laod all custom functions
source(here("src", "load_functions.R"))
load_allCustomFunctions()

############################################################
#~~~~~~~~~~ Generate data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################
cat("\n", ruler, "\n")
cat("Generating data...\n")
cat(ruler, "\n")

# Define data generation parameters
n <- 200     # No. trials
# Parameter list
cat("\nParameters:\n")
par <- list( mu1 = 0.25,  mu2 = 0.25,  # (strong drift)
             boundary = 5, tzero = 0.1)
cat("mu1 = ", par$mu1, "\n")
cat("mu2 = ", par$mu2, "\n")
cat("boundary = ", par$boundary, "\n")
cat("tzero = ", par$tzero, "\n")
cat(sub_ruler, "\n")

# Simulate data
cat("Simulating data with n = ", n, "trials...\n")
cat(ruler, "\n")

# Define path to save data
data_path <- here("output", "toyData.RData")

# Load or generate data
data <- load_or_generate_data(data_path, n, par)

# Show first 10 trials
show <- 10
cat("Data (first ", show, " trials):\n")
head(data, show)

############################################################
#~~~~~~~~~~ Process data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################
# Convert choices to categorical values
data <- add_categorical_choices(data)

# Show first 10 trials with categories
show <- 10
cat("Data with categories (first ", show, " trials):\n")
head(data, show)

plot_choices_circle(data = data, parameter_list = par, cut_points = cut_points)
#~~~~~~~~~~ Fit models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################




