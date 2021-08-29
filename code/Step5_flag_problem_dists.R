

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded.Rds"))


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

# Base theme
base_theme <-   theme(axis.text=element_text(size=6),
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
  theme_bw() + base_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigX_anamalous_distributions.png"),
       width=6.5, height=7.5, units="in", dpi=600)



data_prob <- data %>%
  filter(pdiff>=100)

write.csv(data_prob, file=file.path(datadir, "anamalous_distributions_in_spade_output.csv"), row.names=F)



# Approach #2: Mean relative to EAR (only works for nutrients w/ EAR)
################################################################################

# Build data
##################################

# Cap value
cap <- 5

# Build data
data2 <- data_orig %>%
  # Only nutrients with EAR
  filter(!is.na(ear)) %>%
  # Calculate mean relative to EAR
  mutate(mu_div_ear=mu/ear) %>%
  # Caluclate median (across age groups) of mean relative to EAR for nutrient-country-sex
  group_by(country, nutrient, sex) %>%
  summarize(mu_div_ear=median(mu_div_ear),
            mu_div_ear_cap=pmin(mu_div_ear, cap)) %>%
  ungroup()

# Mark ones to investigate
data2_probs <- data2 %>%
  filter(mu_div_ear>=3)

# Plot data
g2 <- ggplot(data2, aes(y=nutrient, x=country, fill=mu_div_ear_cap)) +
  facet_wrap(~sex) +
  geom_tile(color="grey10") +
  # Porblem points
  geom_point(data=data2_probs, aes(y=nutrient, x=country), inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2(name="Mean intake / EAR",
                       midpoint=1, mid="white", low="navy", high="darkred",
                       lim=c(0, cap),
                       breaks=0:5,
                       labels=c(0:4, ">5")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme
g2

# Export data
ggsave(g2, filename=file.path(plotdir, "FigX_anamalous_distributions2.png"),
       width=6.5, height=3.5, units="in", dpi=600)









