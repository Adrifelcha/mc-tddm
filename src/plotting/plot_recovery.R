plot_recovery <- function(estimates, true_params) {
    # Convert true parameters to match the estimates format  
    true_values <- unlist(par)[c("drift","boundary","tzero","angle")]   

    # Parameter names for plotting
    param_names <- c("Drift Length", "Boundary", "Non-decision Time", "Drift Angle")

    par(mfrow = c(2, 2), mar = c(1, 3, 0, 2))  
    colors <- c("#A5205D", "#205DA5", "#A58020", "#20A55D")
    greek <- c("delta", "eta", "tau", "theta")
    # Loop through parameters and create plots
    for(i in 1:length(param_names)) {
            # Calculate range for y-axis
            y_range <- range(c(estimates[,i], true_values[i]))
            y_margin <- diff(y_range) * 0.2  # Add 20% margin
            
            # Create empty plot
            plot(NA, NA,  xlim = c(0.5, 1.5), axes = F,
                ylim = c(y_range[1] - y_margin, y_range[2] + y_margin),
                xlab = "", ylab = "", xaxt = "n")
            
            # Add Greek letter legend in top right
            legend(0.9, y_range[2]+(y_margin*1.3), legend = parse(text = greek[i]), 
                  bty = "n", text.col = adjustcolor(colors[i], alpha.f = 0.3),
                  cex = 7)  # larger text size
            
            n_ticks <- 5
            axis(2, at = seq(y_range[1], y_range[2], length.out = n_ticks), 
                    labels = round(seq(y_range[1], y_range[2], length.out = n_ticks), 2))

            # Add jittered points with higher transparency
            points(jitter(rep(1, nrow(estimates)), amount = 0.2), 
                estimates[,i], pch = 19,  col = adjustcolor(colors[i], alpha.f = 0.1))  # Increased transparency
            
            # Calculate density
            dens <- density(estimates[,i])
            scaled_density <- (dens$y/max(dens$y)) * 0.2  # Scale density to plot width
            
            # Add mirrored density curves (violin plot style)
            polygon(c(1 - scaled_density, 1 + rev(scaled_density)), 
                c(dens$x, rev(dens$x)),
                col = adjustcolor(colors[i], alpha.f = 0.2),  # Fill color
                border = adjustcolor(colors[i], alpha.f = 0.8))  # Border color
            
            # Add true value as a horizontal line
            abline(h = true_values[i], col = colors[i], lwd = 2, lty = 2)
            
            # Add mean of estimates as a horizontal line
            abline(h = mean(estimates[,i]), col = colors[i], lwd = 2)
            
            # Add quartile lines
            quartiles <- quantile(estimates[,i], probs = c(0.25, 0.75))
            segments(x0 = 0.9, x1 = 1.1, 
                    y0 = quartiles[1], y1 = quartiles[1], 
                    col = colors[i], lwd = 2)
            segments(x0 = 0.9, x1 = 1.1, 
                    y0 = quartiles[2], y1 = quartiles[2], 
                    col = colors[i], lwd = 2)
            
            # Add title with recovery metrics
            bias <- mean(estimates[,i] - true_values[i])
            rmse <- sqrt(mean((estimates[,i] - true_values[i])^2))
            mtext(sprintf("Bias = %.3f", bias), side = 1, line = -2, col = colors[i])
            mtext(sprintf("RMSE = %.3f", rmse), side = 1, line = -3.2, col = colors[i])
    }
}