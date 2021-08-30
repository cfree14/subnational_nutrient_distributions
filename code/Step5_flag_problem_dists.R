

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



# Inspect CV distribution
################################################################################

# Turn off scientific notation
options(scipen=999)

# CV cutoffs
cv_lo <- 0.05
cv_hi <- 3

# CV histogram
g <- ggplot(data_orig, aes(x=cv)) +
  geom_histogram(binwidth=0.05) +
  # Cutoffs
  geom_vline(xintercept=c(cv_lo, cv_hi)) +
  # Labels
  labs(x="Coefficient of variation (CV)", y="Frequency") +
  # Limits
  scale_x_continuous(trans="log2", breaks=c(0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500)) +
  # Theme
  theme_bw()
g


# Inspect low CV values
################################################################################

# Subset
data_cv_lo <- data_orig %>%
  filter(best_dist!="none" & cv < cv_lo)

# Generate densities
data_cv_lo_sim <- nutriR::generate_dists(data_cv_lo) %>%
  # Add ISO3
  mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>%
  # Add simulation id
  mutate(sex_id=recode(sex, "Females"="F", "Males"="M"),
         dist_id=paste0(nutrient, "\n", paste(iso3, sex_id, age_group, sep="-"))) %>%
  # Add CV
  left_join(data_cv_lo %>% select(country, nutrient, sex, age_group, cv))

# Build CV labels
cv_lo_labels <- data_cv_lo_sim %>%
  # Calculate maximum density (y-pos)
  group_by(country, iso3, nutrient, sex, sex_id, age_group, dist_id) %>%
  summarize(density=max(density)) %>%
  ungroup() %>%
  # Add CV
  left_join(data_cv_lo %>% select(country, nutrient, sex, age_group, cv)) %>%
  # Format CV
  mutate(cv_label=round(cv, 4)) %>%
  # Arrange
  arrange(cv) %>%
  # Factor
  mutate(dist_id=factor(dist_id, levels=dist_id))

# Factor
data_cv_lo_sim <- data_cv_lo_sim %>%
  mutate(dist_id=factor(dist_id, levels=cv_lo_labels$dist_id))

# Theme
theme2 <-  theme(axis.text=element_text(size=5),
                 axis.title=element_text(size=7),
                 strip.text=element_text(size=6),
                 # Gridlines
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.position = "bottom")

# Plot lo CVs
g <- ggplot(data_cv_lo_sim, aes(x=intake, y=density, color=cv)) +
  facet_wrap(~dist_id, scales="free", ncol=5) +
  geom_line() +
  # CV value
  geom_text(data=cv_lo_labels, mapping=aes(x=0, y=density, label=cv_label), hjust=0, vjust=1, size=2.3, color="black") +
  # Labels
  labs(x="Habitual intake", y="Density") +
  # Legend
  scale_color_gradientn("Coefficient of variation (CV)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")[3:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme2
g

# Export plot


# Inspect high CV values
################################################################################

# Subset
data_cv_hi <- data_orig %>%
  filter(best_dist!="none" & cv > cv_hi)

# Generate densities
data_cv_hi_sim <- nutriR::generate_dists(data_cv_hi) %>%
  # Add ISO3
  mutate(iso3=countrycode::countrycode(country, "country.name", "iso3c")) %>%
  # Add simulation id
  mutate(sex_id=recode(sex, "Females"="F", "Males"="M"),
         dist_id=paste0(nutrient, "\n", paste(iso3, sex_id, age_group, sep="-"))) %>%
  # Add CV
  left_join(data_cv_lo %>% select(country, nutrient, sex, age_group, cv))

# Build CV labels
cv_hi_labels <- data_cv_hi_sim %>%
  # Calculate maximum density (y-pos)
  group_by(country, iso3, nutrient, sex, sex_id, age_group, dist_id) %>%
  summarize(density=max(density[is.finite(density)])) %>%
  ungroup() %>%
  # Add CV
  left_join(data_cv_hi %>% select(country, nutrient, sex, age_group, cv)) %>%
  # Format CV
  mutate(cv_label=round(cv, 4)) %>%
  # Arrange
  arrange(cv) %>%
  # Factor
  mutate(dist_id=factor(dist_id, levels=dist_id))

# Factor
data_cv_hi_sim <- data_cv_hi_sim %>%
  mutate(dist_id=factor(dist_id, levels=cv_hi_labels$dist_id))

# Theme
theme2 <-  theme(axis.text=element_text(size=5),
                 axis.title=element_text(size=7),
                 strip.text=element_text(size=6),
                 # Gridlines
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 # Legend
                 legend.position = "bottom")

# Plot
g <- ggplot(data_cv_hi_sim, aes(x=intake, y=density, color=cv)) +
  facet_wrap(~dist_id, scales="free", ncol=5) +
  geom_line() +
  # CV value
  geom_text(data=cv_hi_labels, mapping=aes(x=0, y=density, label=cv_label), hjust=0, vjust=1, size=2.3, color="black", inherit.aes=F) +
  # Labels
  labs(x="Habitual intake", y="Density") +
  # Legend
  scale_color_gradientn("Coefficient of variation (CV)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")[3:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme2
g

# Export plot


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


write.csv(data2_probs, file=file.path(datadir, "anamalous_distributions_in_spade_output2.csv"), row.names=F)








