packages <- c("data.table", "ggplot2", "patchwork", "Rcpp", 
              "lubridate", "pracma", "hms", "scales", "psd", 
              "ggridges", "tools", "parallel")

# Check and install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to each package
res <- sapply(packages, install_if_missing)
