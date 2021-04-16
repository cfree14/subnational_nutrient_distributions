

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"

# Read file key
file_key <- read.csv(file.path(datadir, "SPADE_file_key.csv"), as.is=T)


# Build data
################################################################################

# Define age groups
age_group_lo <- c(0, seq(5,100,5))
age_group_hi <- c(seq(4,99,5), Inf)
age_groups <- paste(age_group_lo, age_group_hi, sep="-")
age_group_breaks <- c(-Inf,age_group_hi)

# Loop through files
i <- 1
data_key <- map_df(1:100, function(i) {

  # Read data
  data_orig <- read.csv(file.path(inputdir, file_key$filename[i]), as.is=T)

  # File meta-data
  country <- file_key$country[i]
  iso3 <- file_key$iso3[i]
  nutrient <- file_key$nutrient[i]
  sex <- file_key$sex[i]

  # Format data
  data_key <- data_orig %>%
    # Rename
    rename(id=X, age_yr=age, intake=HI) %>%
    # Add age group
    mutate(age_group=cut(age_yr, breaks=age_group_breaks, labels=age_groups)) %>%
    # Unique age groups
    group_by(age_group) %>%
    summarize(nindivs=n(),
              intake_avg=mean(intake)) %>%
    ungroup() %>%
    # Add other meta data
    mutate(country=country,
           iso3=iso3,
           sex=sex,
           nutrient=nutrient) %>%
    # Arrange
    select(country, iso3, nutrient, sex, age_group, nindivs, intake_avg)

})

# Export data key
write.csv(data_key, file=file.path(datadir, "SPADE_data_key.csv"), row.names=F)


# Quick plot
################################################################################

# Data to plot
data_plot <- data_key %>%
  # Reduce to two sexes
  filter(sex %in% c("Men", "Women")) %>%
  # Order age group
  mutate(age_group=factor(age_group, levels=age_groups))

# Plot data
g <- ggplot(data_plot, aes(x=age_group, y=nutrient)) +
  facet_grid(country ~ sex) +
  geom_raster() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g




