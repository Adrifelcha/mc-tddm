get_choice_in_arc <- function(data, cut_points = c(0, pi/2, pi, 3*pi/2),
                                    categories = c("A", "B", "C", "D")) {
    # If the first cut point is 0, make sure the circle is closed
    if(cut_points[1] == 0){
        if(cut_points[length(cut_points)] != 2*pi){
        cut_points <- c(cut_points, 2*pi)
        }
    }

    # Get the number of choices observed for each category
    category_counts <- table(data$Category)

    # Initialize the angular choice column
    angular_choice <- rep(NA, nrow(data))

    # Assign the angular choice for each category
    from <- 1
    to <- 2
    for(cat in categories){
        # Get a plausible angular choice for the arc region of the category
        if(!is.na(category_counts[cat])){
            arc_position <- runif(category_counts[cat], cut_points[from], cut_points[to])
        } else {
            arc_position <- NA
        }
        # Update the from and to indices
        from <- from + 1
        to <- to + 1
        # Assign the angular choice for the category
        angular_choice[data$Category == cat] <- arc_position
    }

    output <- data.frame("Response" = data$Response, "Category" = data$Category, "Sim_choice" = angular_choice, "RT" = data$RT)
    return(output)
}

