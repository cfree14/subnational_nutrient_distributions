

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

# Read distribution key
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_21countries.Rds")) %>%
  filter(country!="Bulgaria")


# Build data
################################################################################

# Build table
data <- data_orig %>%
  # Stats
  group_by(nutrient, country, age_group) %>%
  summarize(sexes=paste(sort(unique(sex)), collapse="-")) %>%
  ungroup() %>%
  # Recode
  mutate(sexes=recode_factor(sexes,
                            "Males"="Males only",
                            "Females"="Females only",
                            "Females-Males"="Both males and females"))

# Inspect
table(data$sexes)


# Plot data
################################################################################

# Plot
g <- ggplot(data, aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=5) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  # Legend
  scale_fill_ordinal(name="Sexes with data") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=4),
        axis.title=element_text(size=5),
        legend.text=element_text(size=5),
        legend.title=element_text(size=5),
        strip.text=element_text(size=5),
        plot.title=element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"))
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_data_coverage_broad.png"),
       width=6.5, height=8.5, units="in", dpi=600)

