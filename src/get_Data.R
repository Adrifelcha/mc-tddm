# Function to load or generate data
load_or_generate_data <- function(data_path, n, par) {
  if(file.exists(data_path)) {
    # If data exists, check if parameters are the same
    this.par <- par
    load(data_path)
    stored.par <- par
    if(identical(this.par, stored.par)) {
      cat("Data already exists. Loading datafile...\n")
    } else {
      # If parameters are different, generate new data    
      data <- rCDDM_RandomWalk(n, par)
      data <- data$bivariate.data
      save(data, par, file = data_path)
    }
  } else {
    # If data does not exist, generate new data  
    data <- rCDDM_RandomWalk(n, par)
    data <- data$bivariate.data
    save(data, par, file = data_path)
  }
  return(data)
}