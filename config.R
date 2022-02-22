### options
options(readr.num_columns = 0)
options(stringsAsFactors = FALSE)
options(spinner.type = 6)
theme_set(theme_cowplot())
stack_size <- getOption("pandoc.stack.size", default = "1000m")
options(repos = BiocManager::repositories()) # for pushing to shinyapps.io
# options(shiny.reactlog = TRUE) # for checking shiny logic

### folders
rpath <- "R" # additional R code

### general data settings
apptitle_short <- "PFA EPN"
apptitle <- "Pediatric Ependymoma Spatial Transcriptomic Atlas"
url <- "https://www.pneuroonccellatlas.org/"
giturl <- "https://github.com/rnabioco/spatialshiny/"
versionN <- "1.0.0"
geoN <- "GSE195661"
manuscriptL <- "https://github.com/rnabioco/spatialshiny/"
manuscriptD <- "Posterior fossa group A ependymoma (PFA) tumor cellular architecture from 14 patient snap frozen surgical samples (11 primary and 3 matched recurrences) was mapped using the 10x Genomics Visium platform."
manuscriptT <- "Spatial transcriptomic analysis of ependymoma implicates unresolved wound healing as a driver of tumor progression"
pageN <- 10 # number of lines, for tables
warningN <- 100 # number of genes, for throwing warnings in line and heat plots
plot_width <- 4.5
plot_height <- 4
proxy_height <- paste0(plot_height * 100, "px")
proxy_width <- paste0(plot_width * 100, "px")

set_shinytheme <- "paper"
ncore <- parallel::detectCores() - 1
start_tutorial <- TRUE # whether to start loaded app with tutorial