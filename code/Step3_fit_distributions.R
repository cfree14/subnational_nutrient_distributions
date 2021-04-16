

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"

# Read file key
file_key <- read.csv(file.path(datadir, "SPADE_file_key.csv"), as.is=T)

# Merge data
################################################################################

# Define age groups
age_group_lo <- c(0, seq(5,100,5))
age_group_hi <- c(seq(4,99,5), Inf)
age_groups <- paste(age_group_lo, age_group_hi, sep="-")
age_group_breaks <- c(-Inf,age_group_hi)

# Loop through files
i <- j <- 1
fits <- map_df(1:nrow(file_key), function(i) {

  # Read data
  data_orig <- read.csv(file.path(inputdir, file_key$filename[i]), as.is=T)

  # File meta-data
  country <- file_key$country[i]
  iso3 <- file_key$ciso3[i]
  nutrient <- file_key$nutrient[i]
  sex <- file_key$sex[i]

  # Format data
  data <- data_orig %>%
    # Rename
    rename(id=X, age_yr=age, intake=HI) %>%
    # Add age group
    mutate(age_group=cut(age_yr, breaks=age_group_breaks, labels=age_groups))

  # Build distribution fits dataframe
  dist_fits <- data %>%
    select(age_group) %>%
    unique() %>%
    # Add other meta data
    mutate(country=country,
           iso3=iso3,
           sex=sex,
           nutrient=nutrient) %>%
    select(country, iso3, nutrient, sex, age_group) %>%
    # Add columns for distribution fits
    mutate(best_dist=NA,
           g_shape=NA,
           g_rate=NA,
           g_ks=NA,
           ln_meanlog=NA,
           ln_sdlog=NA,
           ln_ks=NA)

  # Loop through age groups
  fits_file <- purrr::map_df(1:nrow(dist_fits), function(j){

    #

    # Fit distributions
    gamma_fit <- try(fitdist(sdata$intake, distr="gamma"))
    lognorm_fit <- try(fitdist(sdata$intake, distr="lnorm"))

    # Plot distributions against data
    # denscomp(list(gamma_fit, lognorm_fit), legendtext = dist_names)

    # Compete distributions (goodness-of-fit)
    # gof_stats <- gofstat(list(gamma_fit, lognorm_fit), fitnames = dist_names)

    # If gamma worked
    if(!inherits(gamma_fit, "try-error")){
      gof_stats <- gofstat(gamma_fit)
      dist_fits$g_ks[i] <- gof_stats$ks
      dist_fits$g_shape[i] <- coef(gamma_fit)["shape"]
      dist_fits$g_rate[i] <- coef(gamma_fit)["rate"]
    }

    # If log-normal worked
    if(!inherits(lognorm_fit, "try-error")){
      gof_stats <- gofstat(lognorm_fit)
      dist_fits$ln_ks[i] <- gof_stats$ks
      dist_fits$ln_meanlog[i] <- coef(lognorm_fit)["meanlog"]
      dist_fits$ln_sdlog[i] <- coef(lognorm_fit)["sdlog"]
    }


  })

  # Return
  fits_file

})
