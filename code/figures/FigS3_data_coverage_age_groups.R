

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_31countries.Rds"))


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
countries_chunked <- split(countries, ceiling(seq_along(countries)/8))
countries1 <- countries_chunked[[1]]
countries2 <- countries_chunked[[2]]
countries3 <- countries_chunked[[3]]
countries4 <- countries_chunked[[4]]

# Theme
my_theme <-   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                    axis.text=element_text(size=5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_blank(),
                    legend.position = "bottom",
                    legend.key.size = unit(0.3, "cm"))

# Plot first
g1 <- ggplot(data %>% filter(country %in% countries1), aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=4) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  # Legend
  scale_fill_ordinal(name="Sexes with data") +
  # Theme
  theme_bw() + my_theme
g1

# Plot second
g2 <- ggplot(data %>% filter(country %in% countries2), aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=4) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  # Legend
  scale_fill_ordinal(name="Sexes with data") +
  # Theme
  theme_bw() + my_theme
g2

# Plot third
g3 <- ggplot(data %>% filter(country %in% countries3), aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=4) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  # Legend
  scale_fill_ordinal(name="Sexes with data") +
  # Theme
  theme_bw() + my_theme
g3

# Plot fourth
g4 <- ggplot(data %>% filter(country %in% countries4), aes(x=age_group, y=nutrient, fill=sexes)) +
  facet_wrap(~country, ncol=4) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  # Legend
  scale_fill_ordinal(name="Sexes with data") +
  # Theme
  theme_bw() + my_theme
g4

# Export data
ggsave(g1, filename=file.path(plotdir, "FigS3a_data_coverage_age_groups.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Export data
ggsave(g2, filename=file.path(plotdir, "FigS3b_data_coverage_age_groups.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Export data
ggsave(g3, filename=file.path(plotdir, "FigS3c_data_coverage_age_groups.png"),
       width=6.5, height=6.5, units="in", dpi=600)

# Export data
ggsave(g4, filename=file.path(plotdir, "FigS3d_data_coverage_age_groups.png"),
       width=6.5, height=6.5, units="in", dpi=600)

