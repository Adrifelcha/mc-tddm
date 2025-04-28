# Function to add categorical choices based on circular data
add_categorical_choices <- function(data, cut_points = c(0, pi/2, pi, 3*pi/2),
                                    categories = c("A", "B", "C", "D")) {
  # Ensure choices are within the modulo range
  data$Choice <- data$Choice %% 2*pi
  
  # Defensive programming
  if(length(cut_points) != length(categories)) {
    stop("The number of cut points must be equal to the number of categories.")
  }

  # Initialize category column
  data$cat <- NA
  
  # Assign categories based on cut points
  for (i in 1:(length(cut_points)-1)) {
    data$cat[data$Choice >= cut_points[i] & data$Choice < cut_points[i+1]] <- categories[i]
  }

  data$cat[data$Choice >= cut_points[length(cut_points)]] <- categories[length(categories)]
  
  return(data)
}