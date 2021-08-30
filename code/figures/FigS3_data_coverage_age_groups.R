

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read distribution key
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries.Rds"))


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
                            "Females-Males"="Both males and females",
                            "Children-Females-Males"="Both males and females"))

# Inspect
table(data$sexes)


# Plot data
################################################################################

# Countries
countries <- sort(unique(data$country))
countries1 <- countries[1:12]

# Plot first half
g1 <- ggplot(data %>% filter(country %in% countries1), aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=4) +
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
g1

# Plot second half
g2 <- ggplot(data %>% filter(!country %in% countries1), aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=4) +
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
g2

# Export data
ggsave(g1, filename=file.path(plotdir, "FigS3a_data_coverage_age_groups.png"),
       width=6.5, height=8.5, units="in", dpi=600)

# Export data
ggsave(g2, filename=file.path(plotdir, "FigS3b_data_coverage_age_groups.png"),
       width=6.5, height=8.5, units="in", dpi=600)

