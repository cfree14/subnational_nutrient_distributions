

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(ggbiplot)


# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/old_nhanes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read nutrient key
nutr_key <- readxl::read_excel(file.path(datadir, "SPADE_nutrient_key.xlsx"))


# Merge data
################################################################################

# Folders
folders <- list.files(inputdir)

# Loop through data
data_orig <- purrr::map_df(folders, function(x) {

  # Files
  files <- list.files(file.path(inputdir, x))

  # Loop through files
  sdata <- purrr::map_df(files, function(y) {

    # Read data
    fdata <- read.csv(file.path(inputdir, x, y)) %>%
      mutate(folder=x, filename=y)

  })

  # Return
  sdata

})

# Build file key
file_key <- data_orig %>%
  # Filenames
  select(filename) %>%
  unique() %>%
  # Break into parts
  tidyr::separate(col=filename, sep="_", into=c("country", "sex", "nutrient"), remove=F) %>%
  # Remove country
  select(-country) %>%
  # Format sex
  mutate(sex=recode(sex, "m"="Males", "w"="Females")) %>%
  # Format nutrient
  mutate(nutrient=nutrient %>% gsub(".csv", "", .) %>% stringr::str_to_sentence(),
         nutrient=nutrient %>% gsub("Vit", "Vitamin ", .) %>% stringr::str_to_title(),
         nutrient=recode(nutrient,
                         "Calc"="Calcium",
                         "Carb"="Carbohydrates",
                         "Fola"="Folate",
                         "Mufa"="Monounsaturated fatty acids",
                         "Niac"="Niacin",
                         "Pufa"="Polyunsaturated fatty acids",
                         "Ribo"="Riboflavin",
                         "Satfat"="Saturated fat",
                         "Thia"="Thiamine",
                         "Vitamin A"="Vitamin A (RAE)")) %>%
  # Add nutrient units
  left_join(nutr_key %>% select(nutrient, units), by="nutrient")

table(file_key$sex)
table(file_key$nutrient)


# Format data
data <- data_orig %>%
  # Rename
  rename(id=X, age_yr=age, intake=HI) %>%
  # Add year
  mutate(year=recode(folder,
                     "07_08"=2007,
                     "09_10"=2009,
                     "11_12"=2011,
                     "13_14"=2013,
                     "15_16"=2015,
                     "17_18"=2017) %>% as.numeric()) %>%
  # Add file info
  left_join(file_key, by="filename") %>%
  # Arrange
  select(year, folder, filename, nutrient, units, sex, age_yr, id, intake, everything())

# Export
saveRDS(data, file=file.path(datadir, "nhanes_spade_output.Rds"))





