get_choice_in_arc <- function(data, cut_points = c(0, pi/2, pi, 3*pi/2),
                                    categories = c("A", "B", "C", "D")) {
    # If the first cut point is 0, make sure the circle is closed
    if(cut_points[1] == 0){
        if(cut_points[length(cut_points)] != 2*pi){
        cut_points <- c(cut_points, 2*pi)
        }
    }

    # Get the number of choices observed for each category
    category_counts <- table(data$cat)

    # Initialize the angular choice column
    angular_choice <- rep(NA, nrow(data))

    # Assign the angular choice for each category
    from <- 1
    to <- 2
    for(cat in categories){
        # Get a plausible angular choice for the arc region of the category
        arc_position <- runif(category_counts[cat], cut_points[from], cut_points[to])
        # Update the from and to indices
        from <- from + 1
        to <- to + 1
        # Assign the angular choice for the category
        angular_choice[data$cat == cat] <- arc_position
    }

    output <- data.frame("arc_choice" = angular_choice, "rt" = data$RT)
    return(output)
}

