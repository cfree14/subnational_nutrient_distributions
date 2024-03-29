

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

# Read data
data_orig <- readRDS(file=file.path(datadir, "percent_overlap_among_age_pairs_31countries.Rds"))


# Build data
################################################################################

# Number of age groups required
nages_req <- 5

# Sample size
n_dists <- data_orig %>%
  # Number of ages for each nutrient-country-sex
  group_by(nutrient, country, sex) %>%
  summarize(nages=n_distinct(age_group1)) %>%
  ungroup() %>%
  # Reduce to groups meeting data requirements
  filter(nages >= nages_req)

# Format data
data <- data_orig %>%
  # Remove age/sex data without enough countries
  left_join(n_dists) %>%
  filter(!is.na(nages)) %>%
  select(-nages)

# Compute stats
stats <- data %>%
  # Format nutrient type
  mutate(nutrient_type=ifelse(nutrient=="Caffeine", "Other macronutrient", nutrient_type),
         nutrient_type=ifelse(nutrient=="Choline", "Vitamin", nutrient_type),
         nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  # Calculate median percent overlap
  group_by(nutrient_type, nutrient, country, iso3, sex) %>%
  summarise(poverlap=median(poverlap, na.rm=T)) %>%
  ungroup() %>%
  # Format sex
  mutate(sex=recode_factor(sex,
                           "Women"="Females",
                           "Men"="Males"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=6),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
g <- ggplot(stats, aes(y=nutrient, x=iso3, fill=poverlap)) +
  facet_grid(nutrient_type~sex, space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Median\npercent overlap", colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS8_similarity_across_ages.png"),
       width=6.5, height=6.5, units="in", dpi=600)



