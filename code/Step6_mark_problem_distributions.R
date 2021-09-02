

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

# Problem key
prob_key <- tibble(nutrient=c(rep("Selenium", 4), "Iodine", "Carbohydrates", "Omega-3 fatty acids"),
                   country=c("Belgium", "Mexico", "Romania", "Sweden", "Denmark", "Belgium", "Denmark"),
                   problem="YES")

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
  left_join(prob_key) %>%
  mutate(status=ifelse(problem=="YES" & !is.na(problem), "Scale problem", status)) %>%
  select(-problem) %>%
  # Arrange
  select(country:ln_sev, status, everything())

# Statuses
table(data$status)

# Export data
saveRDS(data, file=file.path(file.path(datadir, "nutrient_intake_distributions_23countries_expanded_final.Rds")))
