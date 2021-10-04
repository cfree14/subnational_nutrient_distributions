

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
tabledir <- "tables"

# Read file key
data <- read.csv(file=file.path(datadir, "SPADE_file_key.csv"), as.is=T)


# Build table
################################################################################

# Build table
stats <- data %>%
  mutate(nutrient_label=ifelse(!is.na(other_name), paste0(nutrient, " (", other_name, ")"), nutrient)) %>%
  group_by(type1, type2, nutrient_label, units) %>%
  summarize(n=n_distinct(country)) %>%
  ungroup()

# Stats for manuscript
table(stats$type1)

# Export
write.csv(stats, file=file.path(tabledir, "TableS2_nutrient_key.csv"), row.names=F)


