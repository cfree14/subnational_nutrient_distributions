

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

# Read overlap data
data_orig <- readRDS(file=file.path(datadir, "percent_overlap_among_country_pairs.Rds")) %>%
  # Remove Phillipines ---- TEMPORARY!!!! FIX PHILLIPINES!!!
  filter(iso1!="PHL" | iso2=="PHL")

# Read distribution data
dists_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds"))


# Build data
################################################################################

# Number of countries required to be included
ncountries_req <- 5

# Sample size
n_dists <- data_orig %>%
  # Number of countries for each nutrient-sex-age
  group_by(nutrient, sex, age) %>%
  summarize(ncountries=n_distinct(iso1)) %>%
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
  # Aggregate some nutrient types
  mutate(nutrient_type=ifelse(nutrient %in% c("Sugar", "beta-Carotene"), "Other macronutrient", nutrient_type)) %>%
  # Recode nutrient types
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  # Calculate median percent overlap
  group_by(nutrient_type, nutrient, sex, age) %>%
  summarise(poverlap=median(poverlap, na.rm=T)) %>%
  ungroup()


# Examples
################################################################################

# Low overlap: Potassiom Females 45-49
# Medium overlap: Omega-3 fatty acids Males 55-59
# High overlap: Vitamin B-12 Females 50-54

# Distributions to use
dists <- dists_full %>%
  # Add cases
  mutate(case = case_when(nutrient == "Potassium" & sex=="Females" & age_group=="45-49" ~ "Low overlap",
                          nutrient == "Omega-3 fatty acids" & sex=="Males" & age_group=="55-59" ~ "Medium overlap",
                          nutrient == "Vitamin C" & sex=="Males" & age_group=="50-54" ~ "High overlap")) %>%
  # Filter
  filter(!is.na(case))

# Simulates
dists_sim <- nutriR::generate_dists(dists) %>%
  # Add case
  left_join(dists %>% select(nutrient, sex, age_group, case))



# Plot data
################################################################################

# Theme
main_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     axis.text=element_text(size=5),
                     axis.title=element_text(size=6),
                     legend.text=element_text(size=5),
                     legend.title=element_text(size=6),
                     strip.text=element_text(size=6),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.position="bottom")

# Subpanel theme
subpanel_theme <- theme(axis.text=element_text(size=5),
                        axis.title.x=element_text(size=6),
                        axis.title.y=element_blank(),
                        plot.subtitle=element_text(size=5),
                        plot.title=element_text(size=6),
                        plot.tag=element_text(size=8),
                        # Gridlines
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        axis.line = element_line(colour = "black"),
                        legend.position = "none")

# Plot data
g1 <- ggplot(stats, aes(y=nutrient, x=age, fill=poverlap)) +
  facet_grid(nutrient_type~sex, space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="Age group (yr)", y="", tag="A") +
  # Legend
  scale_fill_gradientn(name="Median\npercent overlap", lim=c(0,100),
                       colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + main_theme
g1

# Low overlap
g2 <- ggplot(dists_sim %>% filter(case=="Low overlap"),
             aes(x=intake, y=density, color=country)) +
  geom_line(lwd=0.5) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="Low overlap", subtitle="Potassium intake for 45-49-yr-old women", tag="B") +
  # Theme
  theme_bw() + subpanel_theme
g2

# Medium overlap
g3 <- ggplot(dists_sim %>% filter(case=="Medium overlap"),
             aes(x=intake, y=density, color=country)) +
  geom_line(lwd=0.5) +
  lims(x=c(0,0.5)) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="Medium overlap", subtitle="Omega-3 fatty acids for 55-59-yr-old men", tag="C") +
  # Theme
  theme_bw() + subpanel_theme
g3

# Merge plots
g4 <- ggplot(dists_sim %>% filter(case=="High overlap"),
             aes(x=intake, y=density, color=country)) +
  geom_line(lwd=0.5) +
  lims(x=c(0,750)) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="High overlap", subtitle="Vitamin C for 50-54-yr-old men", tag="D") +
  # Theme
  theme_bw() + subpanel_theme
g4

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4,
                             layout_matrix=matrix(c(1,2,
                                                    1,3,
                                                    1,4), ncol=2, byrow=T),
                             widths=c(0.7, 0.3))


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_similarity_across_countries.png"),
       width=6.5, height=5, units="in", dpi=600)



