

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds"))

# Build data
################################################################################

# Format data
data <- data_orig %>%
  # Simplify
  select(country, iso3, nutrient_type, nutrient, nutrient_units, sex, age_group, mu) %>%
  # Calculate country-nutrient-sex average
  group_by(country, iso3, nutrient_type, nutrient, nutrient_units, sex) %>%
  summarize(mu=mean(mu, na.rm=T)) %>%
  ungroup() %>%
  # Calculate median across countries
  group_by(nutrient_type, nutrient, nutrient_units, sex) %>%
  mutate(med=median(mu)) %>%
  ungroup() %>%
  # Calculate scalar
  mutate(pdiff=(mu-med)/med*100,
         pdiff_cap=pmin(pdiff, 500)) %>%
  # Simplify
  filter(sex!="Children")


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(y=nutrient, x=country, fill=pdiff_cap)) +
  facet_wrap(~sex) +
  geom_tile(color="grey10") +
  # Mark problems>
  geom_point(data %>% filter(pdiff>=100), mapping=aes(y=nutrient, x=country), inherit.aes=F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2(name="% difference of country mean\nfrom the across country median",
                       midpoint=0, mid="white", low="navy", high="darkred") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom")
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigX_anamalous_distributions.png"),
       width=6.5, height=7.5, units="in", dpi=600)







