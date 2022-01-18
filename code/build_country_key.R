

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_32countries.Rds"))

# Read representativeness key
rep_key <- readxl::read_excel(file.path(datadir, "country_representativeness_key.xlsx"))

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
  # Recode countries with multiple pops
  mutate(iso3=gsub("-1|-2", "", iso3),
         country=gsub(" 1| 2", "", country)) %>%
  unique() %>%
  # Add continent
  mutate(continent=countrycode(country, "country.name", "continent")) %>%
  # Add representativeness
  left_join(rep_key %>% select(-country), by="iso3") %>%
  mutate(representativeness=stringr::str_to_sentence(representativeness)) %>%
  # Add HDI
  left_join(hdi %>% select(-country), by="iso3") %>%
  # Add iron category
  mutate(iron_type=recode(hdi_catg,
                          "Low"="Low absorption",
                          "Medium"="Moderate absorption",
                          "High"="High absorption",
                          "Very high"="High absorption")) %>%
  # Add zinc category
  mutate(zinc_type=recode(hdi_catg,
                        "Low"="Unrefined diet",
                        "Medium"="Semi-unrefined diet",
                        "High"="Semi-refined diet",
                        "Very high"="Refined diet")) %>%
  # Arrange
  select(continent, country, iso3, representativeness, hdi_catg, hdi, iron_type, zinc_type, everything()) %>%
  arrange(continent, country)

# Export
write.csv(cntry_key, file=file.path(tabledir, "TableS4_country_key.csv"), row.names = F)

