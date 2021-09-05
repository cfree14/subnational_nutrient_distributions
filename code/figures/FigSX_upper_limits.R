

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded.Rds"))

# Build data
################################################################################

#  Build data
stats <- data_orig %>%
  # TEMPORARY: remove selenium/iodine b/c problems
  filter(!nutrient %in% c("Selenium", "Iodine")) %>%
  # Remove children
  filter(sex!="Children") %>%
  # Recode nutrient and sex
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  # Filter to UL
  filter(!is.na(above_ul)) %>%
  # Mean above UL by country, nutrient, sex (so across age groups)
  group_by(nutrient, country, iso3) %>%
  summarize(above_ul=mean(above_ul)) %>%
  ungroup()

# Country order
cntry_key <- stats %>%
  group_by(iso3, country) %>%
  summarize(above_ul=mean(above_ul)) %>%
  arrange(above_ul)

# Nutrient order
nutrient_key <- stats %>%
  group_by(nutrient) %>%
  summarize(above_ul=mean(above_ul)) %>%
  arrange(above_ul)

# Order data
stats_ordered <- stats %>%
  mutate(iso3=factor(iso3, levels=cntry_key$iso3),
         nutrient=factor(nutrient, levels=nutrient_key$nutrient))


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=5),
                     legend.title=element_text(size=6),
                     strip.text=element_text(size=6),
                     plot.title=element_text(size=7),
                     plot.tag=element_text(size=7),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom")

# Plot above UL raster
g1 <- ggplot(stats_ordered, aes(x=iso3, y=nutrient, fill=above_ul)) +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Mean % at risk\nof adverse effects",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(plot.title=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# Export plot
ggsave(g1, filename=file.path(plotdir, "FigSX_upper_limits.png"),
       width=6.5, height=3.5, units="in", dpi=600)


