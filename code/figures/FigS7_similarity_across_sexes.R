

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
data_orig <- readRDS(file=file.path(datadir, "percent_overlap_among_sex_pairs.Rds"))


# Build data
################################################################################

# Number of countries
ncountries_req <- 3

# Sample size
n_dists <- data_orig %>%
  # Number of countries for each nutrient-sex-age
  group_by(nutrient, age_group) %>%
  summarize(ncountries=n_distinct(country)) %>%
  ungroup() %>%
  # Reduce to groups meeting data requirements
  filter(ncountries >= ncountries_req)

# Format data
data <- data_orig %>%
  # Remove age/sex data without enough countries
  left_join(n_dists) %>%
  filter(!is.na(ncountries)) %>%
  select(-ncountries)

# Compute stats
stats <- data %>%
  # Format nutrient type
  mutate(nutrient_type=recode(nutrient_type,
                              "Sugar"="Other macronutrient",
                              "Carotenoid"="Other macronutrient"),
         nutrient_type=ifelse(nutrient=="Choline", "Vitamin", nutrient_type)) %>%
  # Calculate median percent overlap
  group_by(nutrient_type, nutrient, age_group) %>%
  summarise(poverlap=median(poverlap, na.rm=T)) %>%
  ungroup()


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
g <- ggplot(stats, aes(y=nutrient, x=age_group, fill=poverlap)) +
  facet_grid(nutrient_type~., space="free_y", scales="free_y") +
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
ggsave(g, filename=file.path(plotdir, "FigS7_similarity_across_sexes.png"),
       width=6.5, height=5.5, units="in", dpi=600)



