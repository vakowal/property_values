#####################################
# Emma property values analysis:
# Load packages, define functions, and spell out project globals (including directory paths)
# Ginger Kowal February 2022
#####################################

# Load packages
library(ggplot2)
library(dplyr)

# Custom functions
summ_fun <- function(x) {
  c(min = min(x), max = max(x),
    mean = mean(x), median = median(x),
    std = sd(x), count=length(x))
}

relative_to_2005 <- function(df) {
  for (metric in c('min', 'max', 'mean', 'median', 'std')) {
    baseline_val <- df[df$TaxYear == 2005, metric]
    yearly_vals <- df[, metric]
    change_vec <- (yearly_vals - baseline_val) / abs(baseline_val) * 100
    new_col_name <- paste('perc_change_', metric, sep='')
    df[, new_col_name] <- change_vec
  }
  return(df)
}

# globals
# years when properties were appraised (values in other years are the same, or reflect sale)
appraisal_years <- c(2017, 2013, 2009, 2005)

# Project directories
wd <- list()
wd$raw_data   <- "C:/Users/ginge/Documents/Python/property_values/data/raw/"
wd$processed_data <- "C:/Users/ginge/Documents/Python/property_values/data/processed/"
wd$output <- "C:/Users/ginge/Documents/Python/property_values/output/"