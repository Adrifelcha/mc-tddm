###############################################################################
###############################################################################
#####     A macro function for a short demonstration of the monte carlo
#####       approximation to a categorical implementation of the CDDM
###############################################################################
###############################################################################

run_short_MCroutine <- function(n = 1000, nCat = 10, nIter = 1000, show=TRUE, 
                    data_path = here("output", "toyData.RData"), forceRun = TRUE,
                    par = list(drift = 2.0, angle = 0.2, boundary = 2, tzero = 0.6),
                    fig1_path = here("short-demo", "fig1_choice_data.png"),
                    fig2_path = here("short-demo", "fig2_recovery.png")){

        ############################################################
        #~~~~~~ Generate some dataset~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ############################################################
        par <- complete_par_list(par)

        # Print parameters to the console
        if(show){
                cat("\n", ruler, "\n","Generating data...\n", ruler, "\n",
                    "",sub_ruler, "\n Parameters:\n", sub_ruler,
                    "\n Boundary = ", par$boundary, "\n",
                    "Nondt = ", par$tzero, "\n",
                    "Drift length = ", par$drift, "\n",
                    "Drift angle = ", par$angle, "\n", sub_ruler, "\n",
                    "No. trials = ", n, "\n",
                    "",sub_ruler, "\n", "Preparing data:\n")
        }
        
        # Look for datafile and load if it exists, otherwise generate new data
        data <- load_or_generate_data(data_path, n, par, forceRun = forceRun)
        nRow <- 10  # Number of trials to show
        
        if(show){
                cat("",sub_ruler, "\n", " Data (first", nRow, "trials):\n")
                head(data, nRow)
                cat(sub_ruler, "\n")
        }

        ############################################################
        #~~~ Obtain EZCDDM parameter estimates for comparison ~~~~~~
        ############################################################        
        # Get summary statistics from the data generated
        sumstats <- get_summaryStats(angular_vector = data$Response, rt_vector = data$RT)
        # Compute parameter estimates using the EZCDDM
        ez_params <- ezcddm_getParameters(sumstats)
        if(show){ cat("Reference EZCDDM parameter estimates\n", sub_ruler, "\n")   }

        results <- as.data.frame(rbind(c(par$drift, par$boundary, par$tzero, par$angle),
                                    c(ez_params$drift_length, ez_params$bound, ez_params$ndt, ez_params$drift_angle)))
        results <- round(results, 4)
        colnames(results) <- c("Drift_length", "Boundary", "Non-decision_time", "Drift_angle")
        rownames(results) <- c("True", "EZCDDM")
        if(show){   print(results) 
                    cat(sub_ruler, "\n")        }        

        ############################################################
        #~~~~~~ Categorical implementation: Transform data ~~~~~~~~~
        ############################################################
        categories = LETTERS[1:nCat]   # Obtaing category labels
        # Obtain cut points for the categories (equally spaced on the circle; remove the closed point)
        cut_points = seq(0, 2*pi, length.out = nCat+1)[-(nCat+1)]

        # Convert choices to categorical values
        data <- get_categorical_choices(data, cut_points, categories)

        if(show){
            cat("\n", ruler, "\n","We convert the angular responses into categories...\n", ruler, "\n",
                " No. categories = ", nCat, "\n", 
                "Cut points = ", round(cut_points, 3), "\n", 
                "Categories = ", categories, "\n", sub_ruler, "\n",
                " Data with categories (first", show, "trials):\n")
            head(data, show)
            cat(sub_ruler, "\n","Plot showing choices (angles/categories) on a circle:\n", 
                fig1_path, "\n", sub_ruler, "\n")
        }
        
        png(fig1_path, width = 800, height = 800)
        plot_choices_circle(data = data, parameter_list = par, cut_points = cut_points,
                            categories = categories)
        dev.off()

        ############################################################
        #~~~~~~~~~~ Run Monte Carlo approximation ~~~~~~~~~~~~~~~~~
        ############################################################
        if(show){
        cat("\n", ruler, "\n","Run Monte Carlo approximation...\n", ruler, "\n",
            "We run the Monte Carlo approximation over", nIter, "iterations.\n",
            "1) We ignore the real angular choices\n",
            "2) We replace the categorical choices with random angles within the corresponding arc.\n",
            "3) We estimate the EZCDDM parameters from the resulting dataset.\n",
            "4) We repeat the process", nIter, "times.\n")
        }

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

        update_results <-rbind(results, apply(estimates, 2, mean))
        rownames(update_results) <- c(rownames(results), "MC-EZCDDM")

        if(show){
                cat("",sub_ruler, "\n Results:\n")
                print(update_results)
                cat(sub_ruler, "\n","Plot showing the recovery of the EZCDDM parameters:\n", 
                    fig2_path, "\n", sub_ruler, "\n")
        }

        png(fig2_path, width = 800, height = 800)
        plot_recovery(estimates, par)
        dev.off()
return(update_results)
}
