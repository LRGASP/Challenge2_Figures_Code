# functions to install a package if it's missing
install_if_missing <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

install_if_missing("devtools")

install_github_if_missing <- function(package, repo) {
  if (!requireNamespace(package, quietly = TRUE)) {
    devtools::install_github(repo)
  }
}

packages <- c(
  "LSD",
  "MASS",
  "RColorBrewer",
  "cowplot",
  "devtools",
  "dplyr",
  "ggplot2",
  "ggplotify",
  "ggpointdensity",
  "ggprism",
  "ggpubr",
  "ggridges",
  "ggthemes",
  "grid",
  "gridExtra",
  "pheatmap",
  "plyr",
  "scales",
  "viridis"
)

lapply(packages, install_if_missing)

# Load the packages
lapply(packages, library, character.only = TRUE)

# Local Variables:
# ess-indent-offset: 2
# End:
