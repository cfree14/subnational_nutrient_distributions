

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
inputdir <- "data/dris/raw/efsa"
outputdir <- "data/dris/processed"


# Format ULs
################################################################################

# Read ULs
uls_orig1 <- readxl::read_excel(file.path(inputdir, "EFSA_2018_upper_limits.xlsx"), sheet=1)
uls_orig2 <- readxl::read_excel(file.path(inputdir, "EFSA_2018_upper_limits.xlsx"), sheet=2)

# Format ULs minerals
uls1 <- uls_orig1 %>%
  # Gather
  gather(key="age_group", value="ul", 4:ncol(.)) %>%
  # Reduce
  filter(!is.na(ul))

# Format ULs vitamins
uls2 <- uls_orig2 %>%
  # Gather
  gather(key="age_group", value="ul", 4:ncol(.)) %>%
  # Reduce
  filter(!is.na(ul))

# Merge
uls <- bind_rows(uls1, uls2) %>%
  # Rename
  rename(nutrient_type=type, value=ul) %>%
  # Add a few columns
  mutate(sex="Males/Females",
         stage="None",
         drv_type="UL") %>%
  # Arrange
  select(nutrient_type, nutrient, units, drv_type,
         sex, stage, age_group, value)

# Inspect
str(uls)
sort(unique(uls$nutrient))
sort(unique(uls$units))


# Format others
################################################################################

# Read data
data_orig <- readxl::read_excel(file.path(inputdir, "EFSA_2019_dris_formatted.xlsx"))

# Format data
data <- data_orig %>%
  # Rename
  setNames(c("order", "table", "nutrient_type", "nutrient", "units", "drv_type",
             "sex", "stage", "age_group", "value", "value_note")) %>%
  select(-c(order, table)) %>%
  # Add ULs
  bind_rows(uls) %>%
  # Fix nutrient
  mutate(nutrient=recode(nutrient, "Vitamin A"="Vitamin A (RE)"))

# Inspect
sort(unique(data$nutrient_type))
sort(unique(data$nutrient))
sort(unique(data$units))
sort(unique(data$sex))
sort(unique(data$stage))
sort(unique(data$age_group))





















