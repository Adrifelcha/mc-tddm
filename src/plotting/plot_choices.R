#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function sources all R scripts from a specified directory and its subdirectories
# to load all custom functions into the current R environment.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# MORE DETAILS: 
# The function recursively searches through the specified source directory,
# finding all .R files both in the main directory and in all subdirectories.
# Each R script is then sourced, making all functions defined in those files
# available in the current R session.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
plot_choices_circle <- function(data, parameter_list, cut_points = NULL,
                                categories = NULL, color_palette = NULL,
                                point_size = 1.5, point_alpha = 0.5) {
  ########################################
  # Set up for plotting
  ########################################  
  # Extract boundary from parameter list
  boundary <- parameter_list$boundary

  # Check if cut_points are provided
  if(is.null(cut_points)|is.null(categories)) {
    stop("cut_points and categories must be provided")
  }
  
  # Ensure cut_points are in ascending order
  cut_points <- sort(cut_points)  
  # If the first cut point is 0, make sure the circle is closed
  if(cut_points[1] == 0){
    if(cut_points[length(cut_points)] != 2*pi){
      cut_points <- c(cut_points, 2*pi)
    }
  }
  
  # Set graphical parameters for a square plot with minimal margins
  par(mar = c(2, 2, 2, 2), pty = "s")  # 's' creates a square plotting region
  
  # Identify the cartesian coordinates of the choices
  data_rect <- polarToRect(data$Response, boundary)
  x <- data_rect$x
  y <- data_rect$y

  # Identify the categories of the choices  
  all_cats <- sort(unique(categories))
  
  # Set up default colors if not provided
  if (is.null(color_palette)) {
    default_colors <- c("red", "blue", "green", "purple", "orange", 
                        "cyan", "magenta", "yellow", "darkgreen", "navy",
                        "firebrick", "darkorchid", "darkgoldenrod", "deepskyblue",
                        "darkslategray", "hotpink", "indianred", "limegreen",
                        "mediumorchid", "steelblue")
    color_palette <- setNames(default_colors[1:length(all_cats)], all_cats)
  }
  
  ########################################
  # Start plotting
  ########################################
  # Set up the plot area with just enough margin
  plot_margin <- 1.2 * boundary  # Reduced margin
  plot(0, 0, type = "n", xlim = c(-plot_margin, plot_margin), 
       ylim = c(-plot_margin, plot_margin),
       xlab = "", ylab = "", axes = FALSE, asp = 1)
  
  # Draw colored arcs for each category
  for (i in 1:(length(cut_points)-1)) {
    start_angle <- cut_points[i]
    end_angle <- cut_points[i+1]
    
    # Get the category for this arc
    cat_for_arc <- all_cats[i]
    arc_color <- color_palette[cat_for_arc]
    
    # Draw filled arc
    theta <- seq(start_angle, end_angle, length.out = 100)
    polygon(c(0, boundary * cos(theta), 0), 
            c(0, boundary * sin(theta), 0), 
            col = adjustcolor(arc_color, alpha.f = 0.3), 
            border = NA)
    
    # Draw arc outline
    lines(boundary * cos(theta), boundary * sin(theta), 
          col = arc_color, lwd = 2)
  }
  
  # Draw cutpoints (radial lines)
  for (i in 1:length(cut_points)) {
    angle <- cut_points[i]
    lines(c(0, boundary * cos(angle)), 
          c(0, boundary * sin(angle)), 
          col = "black", lty = 2, lwd = 1)
  }
  
  # Plot points with translucent gray
  points(x, y, pch = 19, col = adjustcolor("gray20", alpha.f = point_alpha), 
         cex = point_size)
  
  # Calculate the center angle for each category
  category_centers <- numeric(length(all_cats))
  for (i in 1:length(all_cats)) {
    start_angle <- cut_points[i]
    end_angle <- cut_points[i+1]
    category_centers[i] <- (start_angle + end_angle) / 2
  }
  
  # Add labels at the center of each sector
  for (i in 1:length(all_cats)) {
    label_at <- polarToRect(category_centers[i], boundary * 0.7)
    text(label_at$x, label_at$y, labels = all_cats[i], 
         font = 2, cex = 1.5, col = "black")
  }
  
  # Add tick marks and labels at cut points
  for (i in 1:length(cut_points)) {
    angle <- cut_points[i]
    # Skip 2*pi if it's the same as 0
    if (i == length(cut_points) && abs(angle - 2*pi) < 1e-10) {
      next
    }
    
    # Draw tick mark
    tick_length <- 0.05 * boundary
    lines(c(boundary*cos(angle), (boundary + tick_length)*cos(angle)), 
          c(boundary*sin(angle), (boundary + tick_length)*sin(angle)), 
          col = "black")
    
    # Add label
    if (angle == 0) {                           label <- "0"
    } else if (abs(angle - pi/2) < 1e-10) {     label <- expression(pi/2)
    } else if (abs(angle - pi) < 1e-10) {       label <- expression(pi)
    } else if (abs(angle - 3*pi/2) < 1e-10) {   label <- expression(3*pi/2)
    } else if (abs(angle - 2*pi) < 1e-10) {     label <- expression(2*pi)
    } else {                                    label <- sprintf("%.2f", angle)
    }
    
    text((boundary + 1.5*tick_length)*cos(angle),
         (boundary + 1.5*tick_length)*sin(angle),
         labels = label, cex = 0.8)
  }
  
  # Add parameter information at the bottom using expression for Greek letters
  mtext(substitute(paste(theta, " = ", theta.val, "     ", 
                         delta, " = ", drift.val, "     ", 
                         "t"[0], " = ", tzero.val, "     ",
                         eta, " = ", boundary.val),
                  list(theta.val = parameter_list$angle,
                       drift.val = parameter_list$drift,
                       tzero.val = parameter_list$tzero,
                       boundary.val = parameter_list$boundary)), 
        side = 1, line = 1, cex = 1.5)
}
#' Example usage:
#' 
#' # Assuming 'data' has columns 'Choice' (angles) and 'cat' (categories)
#  par <- list(theta = 0, drift = 1.5, tzero = 0.1, boundary = 3)
#' # cut_points <- c(0, pi/2, pi, 3*pi/2, 2*pi)
#plot_choices_circle(data = data, parameter_list = par, cut_points = cut_points)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function 1: Plot choices contained in a bivariate data object, on a circle
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.CDDM_choiceData <- function(data,par=NA,choice.col.RGB = c(0.65,0.5,0.15)){
    randomWalk <- is.null(dim(data))
    params.available <- sum(is.na(par)) == 0
    if(randomWalk){
        state  <- data$random.walk
        finalState <- getFinalState(state)
        polar <- rectToPolar(finalState[1,1],finalState[1,2])
        boundary <- round(polar[,"dLength"],2)
        bivariate.data <- data$bivariate.data
        finalT <- bivariate.data$RT
    }else{
          choice  <- data[,1]
          finalT <- data[,2]
          if(!params.available){
              print("Please specify parameter values used in simulation")
          }else{
                boundary <- par$boundary
                finalState <- polarToRect(choice,boundary)
          }
    }
    trials <- length(finalT)
    
    # Formatting and plot settings
    cex.text <- 1
    par(pty="s") 
    pm <- boundary + 0.2     # Set x/y lims
    pi.at <- boundary + 0.3  # Set position of pi indicators 
    
    # Create blank plotting space
    plot(-10:10,-10:10,type="n", ann = FALSE, axes = FALSE,
         xlim=c(-pm,pm),ylim=c(-pm,pm))      
    # Draw base circle
    all.Angles <- seq(0,2*pi,0.001) 
    circle <- polarToRect(all.Angles,boundary)
    points(circle[,1],circle[,2], type="l")
    # Emphasize X and Y coordinates
    abline(h = 0, lty=4, col="gray60")  # X axis
    abline(v = 0, lty=4, col="gray60")  # Y axis
    # Mark response observed
    z <- 40
    rgbCol = as.numeric(choice.col.RGB)
    
    if(trials>z){  factor <- trials/40    }else{    factor <- 5    }
    # Draw final choices
    for(i in 1:trials){
      points(finalState[i,1],finalState[i,2], type = "p", pch =16, cex=2,
             col=rgb(rgbCol[1],rgbCol[2],rgbCol[3],1/factor))
    } 
    # Draw full random walk
    if(randomWalk){
          max.trials.plot = min(c(trials,200))
          color <- "#EEDB1C"
          for(i in 1:max.trials.plot){
              z = round(seq(1,sum(!is.na(state[,1,i])),length.out=75),0)
              points(state[z,,i], type = "l", lwd=2,
                     col=rgb(rgbCol[1],rgbCol[2],rgbCol[3],50/max.trials.plot))
          }
    }
}

