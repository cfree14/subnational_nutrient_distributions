

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
uls <- bind_rows(uls1, uls2)

# Inspect
str(uls)
sort(unique(uls$nutrient))
sort(unique(uls$units))


# Format ULs
################################################################################























