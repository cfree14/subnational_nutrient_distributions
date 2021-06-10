

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
data <- readRDS(file=file.path(datadir, "percent_overlap_among_age_pairs.Rds"))


# Build data
################################################################################

# Compute stats
stats <- data %>%
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
                   strip.text=element_text(size=8),
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
ggsave(g, filename=file.path(plotdir, "FigX_similarity_across_ages.png"),
       width=6.5, height=5, units="in", dpi=600)



