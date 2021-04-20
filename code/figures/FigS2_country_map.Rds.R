

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

# Read countries
world_orig <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Build data
world <- world_orig %>%
  mutate(data_yn=gu_a3 %in% data$iso3)

# Countries with data
centroids <- world %>%
  filter(data_yn) %>%
  sf::st_centroid()

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

# Plot
g <- ggplot() +
  geom_sf(data=world, mapping=aes(fill=subregion, alpha=data_yn), lwd=0.2, show.legend = F) +
  # Plot/label centroids
  geom_sf(data=centroids) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "FigSX_country_map.png"),
       width=6.5, height=3, units="in", dpi=600)
