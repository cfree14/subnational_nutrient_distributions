

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"


# Merge data
################################################################################

# Files
intake_files <- list.files(inputdir)

# Build file key
file_key <- tibble(filename=intake_files) %>%
  # Break filename apart
  mutate(filename_temp=filename) %>%
  mutate(filename_temp=gsub("_h_w_", "_hw_", filename_temp),
         filename_temp=gsub("omega_", "omega", filename_temp)) %>%
  tidyr::separate(col=filename_temp, into=c("country", "sex", "nutrient"), sep="_") %>%
  # Recode country
  mutate(country=stringr::str_to_title(country),
         country=recode(country,
                        "Bang"="Bangladesh",
                        "Bulg"="Bulgaria",
                        "Phil"="Philippines",
                        "Rom"="Romania",
                        "Usa"="United States of America")) %>%
  # Recode sex
  mutate(sex=recode(sex,
                    "c"="Children",
                    "hw"="H women",
                    "m"="Men",
                    "w"="Women")) %>%
  # Recode nutrient
  mutate(nutrient=gsub(".csv", "", nutrient),
         nutrient=recode(nutrient,
                         "adsugar"="Added sugar",
                         "ala"="???????????",
                         "alcohol"="Alcohol",
                         "alphacarot"="alpha-Carotene",
                         "b12"="Vitamin B12",
                         "bcarot"="beta-Carotene",
                         "betacarot"="beta-Carotene",
                         "betacrypt"="???????????",
                         "calc"="Calcium",
                         "carb"="Carbohydrates",
                         "chol"="Cholestral",
                         "cholest"="Cholestral",
                         "cu"="Copper",
                         "energy"="Energy",
                         "fat"="Fat",
                         "fiber"="Fiber",
                         "fola"="Folate",
                         "iod"="Iodine",
                         "iodine"="Iodine",
                         "iron"="Iron",
                         "la"="???????????",
                         "lutzea"="???????????",
                         "lyco"="???????????",
                         "mang"="Manganese",
                         "mg"="Magnesium",
                         "mufa"="???????????",
                         "na"="???????????",
                         "niac"="Niacin",
                         "omega3"="Omega-3 fatty acids",
                         "omega6"="Omega-6 fatty acids",
                         "phos"="Phosphate",
                         "plantomega3"="???????????",
                         "pota"="Potassium",
                         "protein"="Protein",
                         "pufa"="Polyunsaturated fatty acids",
                         "retinol"="???????????",
                         "retol"="???????????",
                         "ribo"="Riboflavin",
                         "satfat"="Saturated fat",
                         "se"="Selenium",
                         "sfa"="Saturated fat",
                         "sucrose"="Sucrose",
                         "sugar"="Sugar",
                         "tfa"="Total fat",
                         "tfat"="Total fat",
                         "theo"="???????????",
                         "thia"="Thiamine",
                         "vita"="Vitamin A",
                         "vitb12"="Vitamin B12",
                         "vitb6"="Vitamin B6",
                         "vitc"="Vitamin C",
                         "vitd"="Vitamin D",
                         "vite"="Vitamin E",
                         "vitk"="Vitamin K",
                         "zinc"="Zinc")) %>%
  # Add ISO3
  mutate(country=countrycode(country, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c")) %>%
  # Arrange
  select(filename, country, iso3, sex, nutrient, everything())


# Inspect
table(file_key$country)
table(file_key$sex)
sort(unique(file_key$nutrient))

# Export file key
write.csv(file_key, file=file.path(datadir, "SPADE_file_key.csv"), row.names=F)
