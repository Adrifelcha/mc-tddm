###############################################################################
###############################################################################
#####     A short demonstration of the monte carlo approximation to a
#####           categorical implementation of the CDDM
###############################################################################
###############################################################################

############################################################
#~~~~~~~~~~ Load libraries and source files ~~~~~~~~~~~~~~~~
############################################################
# R libraries needed
library(here)

# Load main loading function
source(here("src/set_up", "load_functions.R"))
load_allCustomFunctions() # Call the rest of the functions

############################################################
#~~~~~~ Generate some dataset~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################
cat("\n", ruler, "\n","Generating data...\n", ruler, "\n")

# Define data generation parameters
n <- 500     # No. trials
par <- list(drift = 1,         # Drift length
            angle = 2,         # Drift angle
            boundary = 2.5,    # Boundary
            tzero = 0.2)       # Non-decision time
# Add the polar/rectangular coordinates as needed
par <- complete_par_list(par)

# Print parameters to the console
cat("\n", sub_ruler, "\n Parameters:\n", sub_ruler,
    "\n Boundary = ", par$boundary, "\n",
    "Nondt = ", par$tzero, "\n",
    "Drift length = ", par$drift, "\n",
    "Drift angle = ", par$angle, "\n", sub_ruler, "\n",
    "No. trials = ", n, "\n", sub_ruler, "\n\n")

# Define path to save data
data_path <- here("output", "toyData.RData")

cat(sub_ruler, "\n", "Preparing data...\n")
# Look for datafile and load if it exists, otherwise generate new data
data <- load_or_generate_data(data_path, n, par)
cat(sub_ruler, "\n")

show <- 10  # Number of trials to show
cat("Data (first ", show, " trials):\n")
head(data, show)
cat(sub_ruler, "\n\n")

############################################################
#~~~ Obtain EZCDDM parameter estimates for comparison ~~~~~~
############################################################
cat(sub_ruler, "\n", "Reference EZCDDM parameter estimates\n")
# Get summary statistics from the data generated
sumstats <- get_summaryStats(angular_vector = data$Response, rt_vector = data$RT)
# Compute parameter estimates using the EZCDDM
ez_params <- ezcddm_getParameters(sumstats)
cat(sub_ruler, "\n")
cat(sub_ruler, "\n")

############################################################
#~~~~~~ Categorical implementation: Transform data ~~~~~~~~~
############################################################
cat("\n", ruler, "\n","Responses are converted into categories...\n", ruler, "\n")

cut_points = c(0, pi/2, pi, 3*pi/2)
categories = c("A", "B", "C", "D")
# Convert choices to categorical values
data <- get_categorical_choices(data, cut_points, categories)

cat("Data with categories (first ", show, " trials):\n")
head(data, show)

fig1_path <- here("short-demo", "fig1_choice_data.png")
cat(sub_ruler, "\n","Create a plot showing choices generated on a circle:\n", 
    fig1_path, "\n", sub_ruler)
png(fig1_path, width = 800, height = 800)
plot_choices_circle(data = data, parameter_list = par, cut_points = cut_points)
dev.off()

############################################################
#~~~~~~~~~~ Run Monte Carlo approximation ~~~~~~~~~~~~~~~~~
############################################################
nIter <- 1000

cat("\n", ruler, "\n","Run Monte Carlo approximation...\n", ruler, "\n", sub_ruler, "\n", 
    "We run the Monte Carlo approximation over", nIter, "iterations.\n")

estimates <- matrix(NA, nrow = nIter, ncol = 4)
for(i in 1:nIter){
    set.seed(i) # Set seed for reproducibility
    tmp <- get_choice_in_arc(data, cut_points, categories)
    tmp_sumstats <- get_summaryStats(angular_vector = tmp$Sim_choice, rt_vector = tmp$RT)
    tmp_estimates <- ezcddm_getParameters(tmp_sumstats)
    estimates[i, ] <- c(tmp_estimates$drift_length, tmp_estimates$bound, tmp_estimates$ndt, tmp_estimates$drift_angle)
}

estimates <- as.data.frame(estimates)
colnames(estimates) <- c("drift_length", "bound", "ndt", "drift_angle")



rbind(apply(estimates, 2, mean), ez_params, c(par$drift, par$boundary, par$tzero, par$angle))



png(here("short-demo", "fig2_recovery.png"), width = 800, height = 800)
plot_recovery(estimates, par)
dev.off()