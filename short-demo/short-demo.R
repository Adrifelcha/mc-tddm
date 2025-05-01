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
source(here("src/set_up", "load_functions.R"))
load_allCustomFunctions()

############################################################
#~~~~~~~~~~ Generate data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################
cat("\n", ruler, "\n")
cat("Generating data...\n")
cat(ruler, "\n")

# Define data generation parameters
n <- 500     # No. trials
# Parameter list
cat("\nParameters:\n")
par <- list(mu1 = 0.4,  mu2 = 0.3,  # (strong drift)
            boundary = 5, tzero = 0.1)
par$drift <- rectToPolar(par$mu1, par$mu2)$dLength
par$angle <- rectToPolar(par$mu1, par$mu2)$dAngle


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
cut_points = c(0, pi/2, pi, 3*pi/2)
categories = c("A", "B", "C", "D")
# Convert choices to categorical values
data <- get_categorical_choices(data, cut_points, categories)

# Show first 10 trials with categories
show <- 10
cat("Data with categories (first ", show, " trials):\n")
head(data, show)

png(here("short-demo", "fig1_choice_data.png"), width = 800, height = 800)
plot_choices_circle(data = data, parameter_list = par, cut_points = cut_points)
dev.off()

############################################################
#~~~~~~~~~~ Run Monte Carlo approximation ~~~~~~~~~~~~~~~~~
############################################################
nIter <- 1000

estimates <- matrix(NA, nrow = nIter, ncol = 4)
for(i in 1:nIter){
    output <- get_choice_in_arc(data, cut_points, categories)
    sumStats <- get_summaryStats(angular_vector = output$arc_choice, rt_vector = output$rt)
    ez_params <- ezcddm_getParameters(sumStats)
    estimates[i, ] <- c(ez_params$drift_length, ez_params$bound, ez_params$ndt, ez_params$drift_angle)
}

estimates <- as.data.frame(estimates)
colnames(estimates) <- c("drift_length", "bound", "ndt", "drift_angle")

png(here("short-demo", "fig2_recovery.png"), width = 800, height = 800)
plot_recovery(estimates, par)
dev.off()