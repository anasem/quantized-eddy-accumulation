message("Sourcing dependencies")

project_dir     <- "~/Projects/improved-REA/public-repo"
setwd(project_dir)

# List of packages to be loaded
packages <- c("data.table", "ggplot2", "patchwork", "Rcpp", 
              "lubridate", "pracma", "hms", "scales", "psd", 
              "ggridges", "tools", "parallel")

# Check if packages are installed, and show an error message if not
load_package <- function(package) {
  if (!require(package, character.only = TRUE)) {
    stop(paste0("Package '", package, "' is not installed. Please install it."))
  } else {
    message(paste0("Loading package '", package, "'."))
  }
}


# Load required packages
res <- lapply(packages, function(pkg) suppressPackageStartupMessages(load_package(pkg)))


# Source functions
source(file.path(project_dir, "src/simulation/functions.R"))
sourceCpp(file.path(project_dir, "src/simulation/quantize-w.cpp"))

