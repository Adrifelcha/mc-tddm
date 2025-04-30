###############################################################################
# Load or Generate Bivariate Data from the CDDM: ##############################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function handles data management for CDDM simulations:
# 1. Checks if data exists at the specified path
# 2. If data exists, verifies parameters match the requested parameters
# 3. Loads existing data if parameters match
# 4. Generates new data if file doesn't exist or parameters differ
# 5. Saves newly generated data for future use
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

# Function to load or generate data
load_or_generate_data <- function(data_path, n, par) {
    # Check if the data file already exists
    if(file.exists(data_path)) {
      # If data exists, check if parameters are the same
      this.par <- par
      load(data_path)
      stored.par <- par
      if(identical(this.par, stored.par)) {
        cat("Data already exists. Loading datafile...\n")
      } else {
        # If parameters differ, generate new data with current parameters
        cat("Parameters changed. Generating new data...\n")
        data <- rCDDM_RandomWalk(n, par)
        data <- data$bivariate.data
        save(data, par, file = data_path)
      }
    } else {
      # If data file doesn't exist, generate and save new data
      cat("Data file not found. Generating new data...\n")
      data <- rCDDM_RandomWalk(n, par)
      data <- data$bivariate.data
      save(data, par, file = data_path)
    }
  
  return(data)
}