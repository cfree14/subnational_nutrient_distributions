

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(wbstats)
library(countrycode)

# Directories
datadir <- "data/population"

# WB countries
wb_cntry_df_full <- wbcountries()
wb_cntry_df <- wb_cntry_df_full %>% filter(region!="Aggregates")



# Format WB age/sex population data (past)
################################################################################

# WP population indicators
wbi <- wbindicators()
wbi_pop <- wbi %>%
  filter(grepl("SP.POP", indicatorID))

# Identify indicator codes for population by age/sex
age_ids <- paste0("SP.POP.", c("0004", "0509", "1014", "1519", "2024",
                               "2529", "3034", "3539", "4044", "4549", "5054",
                               "5559", "6064", "6569", "7074", "7579", "80UP"))
age_sex_ids <- sort(c(paste0(age_ids, ".FE"), paste0(age_ids, ".MA")))

# 2020 population by country and age/sex
data_orig <- wb_data(country=wb_cntry_df$iso3c, indicator = age_sex_ids , start_date = 2019, end_date = 2019, return_wide = F)

# Country key
wb_cntry_key <- data_orig %>%
  # Reduce to unique countries
  select(country, iso3c, iso2c) %>%
  unique() %>%
  rename(country_orig=country, iso3_orig=iso3c, iso2_orig=iso2c) %>%
  # Format ISO3/country names
  mutate(country_use=countrycode(iso3_orig, "iso3c", "country.name"),
         iso3_use=countrycode(country_use, "country.name", "iso3c")) %>%
  # Replace missing values
  mutate(country_use=ifelse(is.na(country_use), country_orig, country_use),
         iso3_use=ifelse(is.na(iso3_use), iso3_orig, iso3_use)) %>%
  # Arrange
  select(country_orig, iso3_orig, iso2_orig, country_use, iso3_use)

# Format data
data <- data_orig %>%
  # Rename columns
  rename(country_orig=country, iso3_orig=iso3c, iso2_orig=iso2c, year=date, pop_size=value) %>%
  # Format year
  mutate(year=as.numeric(year)) %>%
  # Add formatted ISO/country
  left_join(wb_cntry_key %>% select(country_orig, country_use, iso3_use), by="country_orig") %>%
  # Add age and sex
  mutate(sex=ifelse(grepl("female", indicator), "female", "male"),
         age=gsub("Population ages |, female|, male", "", indicator)) %>%
  # Format age
  mutate(age=recode(age,
                    "00-04"="0-4",
                    "05-09"="5-9",
                    "80 and above"="80+"),
         age=factor(age, levels=c("0-4", "5-9", "10-14", "15-19", "20-24",
                                  "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                  "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))) %>%
  # Arrange
  select(country_orig, iso3_orig, iso2_orig, country_use, iso3_use, year,
         indicator_id, indicator, sex, age, sex,
         pop_size, everything()) %>%
  select(-c(unit:last_updated)) %>%
  arrange(country_use, year, age, sex)


# Export
################################################################################

# Export data
saveRDS(data, file=file.path(datadir, "WB_2019_population_size_by_country_agesex.Rds"))
write.csv(data, file=file.path(datadir, "WB_2019_population_size_by_country_agesex.csv"))

