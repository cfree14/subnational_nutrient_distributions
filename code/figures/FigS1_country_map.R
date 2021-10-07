

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

# Read country key
data <- read.csv(file=file.path(tabledir, "TableS4_country_key.csv"), as.is=T)

# Read countries
world_orig <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Build data
world <- world_orig %>%
  # (data_yn=gu_a3 %in% data$iso3)
  left_join(data %>% select(iso3, representativeness), by=c("gu_a3"="iso3"))

# Countries with data
centroids <- world %>%
  filter(!is.na(representativeness)) %>%
  sf::st_centroid() %>%
  mutate(long_dd=sf::st_coordinates(.)[,1],
         lat_dd=sf::st_coordinates(.)[,2])

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
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "bottom")

# Plot
g <- ggplot() +
  geom_sf(data=world, lwd=0.2, fill="grey90", color="white") +
  geom_sf(data=world %>% filter(!is.na(representativeness)), mapping=aes(fill=representativeness), lwd=0.2, color="white") +
  # Plot/label centroids
  # geom_sf(data=centroids, size=1.2) +
  ggrepel::geom_text_repel(data=centroids, mapping=aes(x=long_dd, y=lat_dd, label=geounit),
                           min.segment.length = 0, size=2, max.overlaps = 999) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend
  scale_fill_discrete(name="Representativeness", na.value="grey90") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_country_map.png"),
       width=6.5, height=3.25, units="in", dpi=600)
