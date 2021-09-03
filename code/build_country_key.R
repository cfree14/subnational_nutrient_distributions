

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)
library(fitdistrplus)
library(wbstats)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries.Rds"))

# Read Human Development Index
# http://hdr.undp.org/en/content/human-development-index-hdi
hdi_orig <- readxl::read_excel("data/human_development_index/2020_statistical_annex_all.xlsx", sheet=2)


# Format HDI
################################################################################

# Format HDI
hdi <- hdi_orig %>%
  # Remove header
  slice(8:nrow(.)) %>%
  # Remove footer
  slice(1:192) %>%
  # Simplify
  select(2:3) %>%
  # Rename
  setNames(c("country", "hdi")) %>%
  # Filter again
  filter(!is.na(hdi)) %>%
  # Convert
  mutate(hdi=as.numeric(hdi)) %>%
  # Add categories
  # Very high (>0.8), high (0.7-0.8), medium (0.55-0.7), low (<0.55)
  mutate(hdi_catg=cut(hdi, breaks=c(0, 0.55, 0.7, 0.8,1),
                      labels=c("Low", "Medium", "High", "Very high"))) %>%
  # Correct countries
  mutate(iso3=countrycode(country, "country.name", "iso3c"),
         country=countrycode(iso3, "iso3c", "country.name"))

# Build country key
################################################################################

# Zinc: low, moderate, high absorption
# Iron: unrefined, semi-unrefined, semi-refined, refined

# Country key
cntry_key <- data_orig %>%
  # Unique countries
  select(country, iso3) %>%
  unique() %>%
  # Add continent
  mutate(continent=countrycode(country, "country.name", "continent")) %>%
  # Arrange
  select(continent, country, iso3) %>%
  arrange(continent, country) %>%
  # Add HDI
  left_join(hdi) %>%
  # Add iron category
  mutate(iron_type=recode(hdi_catg,
                          "Low"="Low absorption",
                          "Medium"="Moderate absorption",
                          "High"="High absorption",
                          "Very high"="High absorption")) %>%
  # Add zinc category
  mutate(zinc_type=recode(hdi_catg,
                        "Low"="Unrefined",
                        "Medium"="Semi-unrefined",
                        "High"="Semi-refined",
                        "Very high"="Refined"))

# Export
write.csv(cntry_key, file=file.path(tabledir, "TableS4_country_key.csv"), row.names = F)

