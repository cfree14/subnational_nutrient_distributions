

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
# inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
inputdir <- "~/Desktop/all_intakes" # one time only
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read file key
file_key <- read.csv(file.path(datadir, "SPADE_file_key.csv"), as.is=T)

# Read nutrient key
nutr_key <- readxl::read_excel(file.path(datadir, "SPADE_nutrient_key.xlsx"))


# Function to fit data
################################################################################

# Fit distributions
country <- "Belgium"
fit_dists <- function(country){

  # Country
  country_do <- country
  files_do <- file_key %>%
    filter(country==country_do)
  iso_do <- unique(files_do$iso3)

  # Define age groups
  age_group_lo <- c(0, seq(5,100,5))
  age_group_hi <- c(seq(4,99,5), Inf)
  age_groups <- paste(age_group_lo, age_group_hi, sep="-")
  age_group_breaks <- c(-Inf,age_group_hi)

  # Loop through files
  # i <- j <- 1
  # for(i in 1:nrow(files_do)){
  fits_cntry <- map_df(1:nrow(files_do), function(i) {

    # Read data
    file_do <- files_do$filename[i]
    data_orig <- read.csv(file.path(inputdir, file_do), as.is=T)

    # File meta-data
    sex_do <- files_do$sex[i]
    nutrient_do <- files_do$nutrient[i]

    # Format data
    data <- data_orig %>%
      # Rename
      rename(id=X, age_yr=age, intake=HI) %>%
      # Add age group
      mutate(age_group=cut(age_yr, breaks=age_group_breaks, labels=age_groups))

    # Loop through age groups
    age_groups_do <- unique(data$age_group)
    fits_file <- purrr::map_df(1:length(age_groups_do), function(j){

      # Subset data
      age_group_do <- age_groups_do[j]
      sdata <- data %>%
        filter(age_group == age_group_do)

      # Fit distributions
      gamma_fit <- try(fitdist(sdata$intake, distr="gamma"))
      lognorm_fit <- try(fitdist(sdata$intake, distr="lnorm"))

      # Plot distributions against data
      # dist_names <- c("gamma", "log-normal")
      # denscomp(list(gamma_fit, lognorm_fit), legendtext = dist_names)

      # Compete distributions (goodness-of-fit)
      # gof_stats <- gofstat(list(gamma_fit, lognorm_fit), fitnames = dist_names)

      # Setup container
      fits_age <- tibble(country=country_do,
                         iso3=iso_do,
                         nutrient=nutrient_do,
                         sex=sex_do,
                         age_group=age_group_do,
                         best_dist=NA,
                         g_shape=NA,
                         g_rate=NA,
                         g_ks=NA,
                         ln_meanlog=NA,
                         ln_sdlog=NA,
                         ln_ks=NA)

      # If gamma worked
      if(!inherits(gamma_fit, "try-error")){
        gof_stats <- gofstat(gamma_fit)
        fits_age$g_ks <- gof_stats$ks
        fits_age$g_shape <- coef(gamma_fit)["shape"]
        fits_age$g_rate <- coef(gamma_fit)["rate"]
      }

      # If log-normal worked
      if(!inherits(lognorm_fit, "try-error")){
        gof_stats <- gofstat(lognorm_fit)
        fits_age$ln_ks <- gof_stats$ks
        fits_age$ln_meanlog <- coef(lognorm_fit)["meanlog"]
        fits_age$ln_sdlog <- coef(lognorm_fit)["sdlog"]
      }

      # Return
      fits_age


    })

    # Return
    fits_file

  # }
  })

  # Mark best distribution
  # The smaller the K-S statistic, the better
  fits_out <- fits_cntry %>%
    mutate(best_dist=ifelse(is.na(g_ks) & is.na(ln_ks), "none",
                            ifelse(ln_ks < g_ks | is.na(g_ks), "log-normal", "gamma")))

  # Export
  outfile <- paste0(country, ".rds") %>% tolower() %>% gsub(" ", "_", .)
  saveRDS(fits_out, file.path(outdir, outfile))

  # Return
  return(fits_out)

}


# Fit distributions
################################################################################

sort(unique(file_key$country))

# Asia
bang <- fit_dists(country="Bangladesh")
chin <- fit_dists(country="China")
laos <- fit_dists(country="Laos")
phil <- fit_dists(country="Philippines")

# Americas
braz <- fit_dists(country="Brazil")
can <- fit_dists(country="Canada")
mex <- fit_dists(country="Mexico")
usa <- fit_dists(country="United States")

# Africa
faso <- fit_dists(country="Burkina Faso")
eth <- fit_dists(country="Ethiopia")
ken <- fit_dists(country="Kenya")
uga <- fit_dists(country="Uganda")
zam <- fit_dists(country="Zambia")

# Europe
bulg <- fit_dists(country="Bulgaria")
esto <- fit_dists(country="Estonia")
ital <- fit_dists(country="Italy")
neth <- fit_dists(country="Netherlands")
port <- fit_dists(country="Portugal")
rom <- fit_dists(country="Romania")
swe <- fit_dists(country="Sweden")
belg <- fit_dists(country="Belgium")
bosn <- fit_dists(country="Bosnia & Herzegovina")
den <- fit_dists(country="Denmark")


# Merge distribution fits
################################################################################

# Merge data
files2merge <- list.files(outdir)
ncountries <- length(files2merge)
data <- purrr::map_df(files2merge, function(x) {
  fdata <- readRDS(file.path(outdir, x))
})

# Format
data_out <- data %>%
  # Remove 100+ category
  filter(age_group!="100-Inf") %>%
  mutate(age_group=droplevels(age_group)) %>%
  # Fill missing best distributions
  mutate(best_dist=ifelse(is.na(best_dist) & !is.na(g_ks), "gamma", best_dist) %>% as.character()) %>%
  # Convert to numeric
  mutate(g_shape=as.numeric(g_shape),
         g_rate=as.numeric(g_rate),
         g_ks=as.numeric(g_ks),
         ln_meanlog=as.numeric(ln_meanlog),
         ln_sdlog=as.numeric(ln_sdlog),
         ln_ks=as.numeric(ln_ks)) %>%
  # Add nutrient info
  left_join(nutr_key %>% dplyr::select(nutrient, type2, units)) %>%
  # Arrange
  dplyr::select(country, iso3, type2, nutrient, units, everything()) %>%
  rename(nutrient_type=type2, nutrient_units=units)

# Inspect
str(data_out)
freeR::complete(data_out)
levels(data_out$age_group)

# Export merged data
outfile <- paste0("nutrient_intake_distributions_", ncountries, "countries.Rds")
saveRDS(data_out, file=file.path(datadir, outfile))

