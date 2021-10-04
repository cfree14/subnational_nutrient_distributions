

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded.Rds"))


# Read data
################################################################################

# Problem distributions
# Selenium: Belgium, Mexico, Romania, Sweden
# Iodine: Denmark
# Carbohydrates: Belgium
# Omega-3 fatty acids: Denmark

# Example of how to build a problem key if necessary
# prob_key <- tibble(nutrient=c(rep("Selenium", 4), "Iodine", "Carbohydrates", "Omega-3 fatty acids"),
#                    country=c("Belgium", "Mexico", "Romania", "Sweden", "Denmark", "Belgium", "Denmark"),
#                    problem="YES")

# CV cutoff
cv_lo_final <- 0.02

# Format data
data <- data_orig %>%
  # Distribution status
  mutate(status=recode(best_dist,
                       "gamma"="Fit",
                       "log-normal"="Fit",
                       "none"="Failed to fit"),
         status=ifelse(cv<cv_lo_final & !is.na(cv), "Not representative", status)) %>%
  # Mark problems
  # left_join(prob_key) %>%
  # mutate(status=ifelse(problem=="YES" & !is.na(problem), "Scale problem", status)) %>%
  # select(-problem) %>%
  # Arrange
  select(-c(ear_preg, ear_lact, nutrient_ar)) %>%
  select(continent, country, iso3,
         hdi_catg, hdi,
         iron_type, zinc_type,
         nutrient_type, nutrient, nutrient_units,
         sex, age_group, sex_ear:ear, sex_ar:ar,
         age_group_ul, ul_h, ul_h_units,
         g_shape:g_sev_ar, g_above_ul,
         ln_meanlog:ln_sev_ar, ln_above_ul,
         status, best_dist, mu:kurt, above_ul, sev_ear, sev_ar, ear_use, sev,
         cutpoint_sev_ear:cutpoint_sev,
         everything())

# Statuses
table(data$status)
colnames(data)

# Export data
saveRDS(data, file=file.path(file.path(datadir, "nutrient_intake_distributions_23countries_expanded_final.Rds")))

