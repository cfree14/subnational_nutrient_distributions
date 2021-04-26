
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read GENUS data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/nutrient_endowment/data/genus/processed/genus_edible_food_by_cntry_year.Rds")

# Plot data
################################################################################

# Countries to compare
countries <- c("United States", "Canada")

# Foods
foods <- sort(unique(data_orig$food))

# Format data
data <- data_orig %>%
  # Reduce to countries of interest
  filter(iso3_use %in% c("USA", "CAN") & year==max(year)) %>%
  filter(!is.na(g_person_day)) %>%
  # Reduce to first 20 foods
  filter(food %in% foods[1:40])

my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.title=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position=c(0.75, 0.85),
                  legend.background = element_rect(fill=alpha('blue', 0)),
                  legend.key.size = unit(0.5, "cm"))

# Plot data
g <- ggplot(data, aes(y=food, x=g_person_day, fill=country_use)) +
  geom_bar(stat="identity", position="dodge") +
  # Labels
  labs(y="", x="Food supply (g/p/d)", title="40 of 225 foods shown") +
  # Legend
  scale_fill_discrete(name="Country") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_intake_group_matching_example.png"),
       width=3.5, height=3.5, units="in", dpi=600)
