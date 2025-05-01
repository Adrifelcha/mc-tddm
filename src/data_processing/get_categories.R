# Function to add categorical choices based on circular data
get_categorical_choices <- function(data, cut_points = c(0, pi/2, pi, 3*pi/2),
                                    categories = c("A", "B", "C", "D")) {
    # Ensure choices are within the (0,2pi) range
    data$Choice[1:10] <- data$Choice[1:10] %% (2*pi)
    
    # If the first cut point is 0, make sure the circle is closed
    if(cut_points[1] == 0){
      if(cut_points[length(cut_points)] != 2*pi){
        cut_points <- c(cut_points, 2*pi)
      }
    }

    # Categorize responses based on cut points
    data$cat <- NA  
    for (i in 1:(length(cut_points))) {
        data$cat[data$Choice >= cut_points[i] & data$Choice < cut_points[i+1]] <- categories[i]
    }
    
return(data)
}