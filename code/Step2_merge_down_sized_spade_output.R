

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read file key
file_key <- read.csv(file.path(datadir, "SPADE_file_key.csv"), as.is=T)

# Read nutrient key
nutr_key <- readxl::read_excel(file.path(datadir, "SPADE_nutrient_key.xlsx"))

# Merge data
################################################################################

# Define age groups
age_group_lo <- c(0, seq(5,100,5))
age_group_hi <- c(seq(4,99,5), Inf)
age_groups <- paste(age_group_lo, age_group_hi, sep="-")
age_group_breaks <- c(-Inf,age_group_hi)

# Loop through files and merge
data_merge <- purrr::map_df(1:nrow(file_key), function(x){

  # Read file
  filename <- file_key$filename[x]
  fdata_orig <- read.csv(file.path(inputdir, filename))

  # Parameters
  iso3 <- file_key$iso3[x]
  country <- file_key$country[x]
  sex <- file_key$sex[x]
  nutrient <- file_key$nutrient[x]
  units <- file_key$units[x]

  # How many indivs per age group to retain?
  n_per_group_max <- 10000

  # Format file
  fdata <- fdata_orig %>%
    # Rename
    rename(id=X, age_yr=age, intake=HI) %>%
    # Add age group
    mutate(age_group=cut(age_yr, breaks=age_group_breaks, labels=age_groups)) %>%
    # Add other useful columns
    mutate(country=country,
           iso3=iso3,
           sex=sex,
           nutrient=nutrient,
           units=units) %>%
    # Arrange
    # This ignores "amount" and "freq" in Uganda Females Vitamin B12
    select(nutrient, units, country, iso3, sex, age_group, age_yr, id, intake) %>%
    arrange(age_yr, id) %>%
    # Subsample if necessary
    group_by(age_group) %>%
    mutate(n_in_group=length(id)) %>%
    sample_n(pmin(n_in_group, n_per_group_max)) %>%
    ungroup() %>%
    select(-n_in_group)

})

# Format data (if you want to make smaller?)
data_out <- data_merge %>%
  select(-c(country, units, age_yr, id))

# Export data
saveRDS(data_merge, file.path(datadir, "SPADE_output_merged_but_downscaled.Rds"))

