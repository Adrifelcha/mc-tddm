#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This function sources all R scripts from the /src/ directory,
# including all existing subdirectories (if any).
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

# Rulers for printing results to the console
# Passed to the function, but kept in the global environment for convenience
ruler <- paste(rep("=", 60), collapse = "")
sub_ruler <- paste(rep("-", 40), collapse = "")

load_allCustomFunctions <- function(source_dir = here("src"), show = TRUE) {
  ruler <- paste(rep("=", 60), collapse = "")
  sub_ruler <- paste(rep("-", 40), collapse = "")

  cat("\n", ruler, "\n")
  cat(" LOADING CUSTOM FUNCTIONS FROM:\n")
  cat(paste(" ", source_dir, "\n"))
  cat(ruler, "\n")
  
  # Get all directories within source folder
  src_dirs <- list.dirs(source_dir, recursive = FALSE)
  
  # Loop through each directory
  for (dir_path in src_dirs) {
    dir_name <- basename(dir_path)
    if(show){
              cat(sub_ruler, "\n")
              cat(paste(" Subdirectory:", dir_name, "\n"))
              cat(sub_ruler, "\n")
    }
    
    # Get all R files in the current directory
    r_files <- list.files(dir_path, pattern = "\\.R$", full.names = TRUE)
    
    # Source each R file
    for (r_file in r_files) {
      file_name <- basename(r_file)
      if(show){ cat(paste("  Sourcing:", file_name, "\n"))  }
      source(r_file)
    }
    
    if(show){ cat("\n") } # Add vertical space between directories
  }
  
  # Also source any R files directly in the source directory
  r_files_root <- list.files(source_dir, pattern = "\\.R$", full.names = TRUE)
  if (length(r_files_root) > 0) {
    if(show){
      cat(sub_ruler, "\n")
      cat(paste(" Root directory:", basename(source_dir), "\n"))
      cat(sub_ruler, "\n")
    }
    
    for (r_file in r_files_root) {
      file_name <- basename(r_file)
      if(show){ cat(paste("  Sourcing:", file_name, "\n"))  }
      source(r_file)
    }
    
    if(show){ cat("\n") }
  }
  
  cat(" All custom functions loaded successfully!\n")
}