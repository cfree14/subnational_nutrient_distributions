

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
data_orig <- readRDS(file=file.path(datadir, "percent_overlap_among_country_pairs.Rds"))

# Read distribution data
dists_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded_final.Rds")) %>%
  filter(best_dist!="none" & status!="Not representative")


# Build data
################################################################################

# Number of countries required to be included
ncountries_req <- 3

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

# Distributions to use
dists <- dists_orig %>%
  # Add cases
  mutate(case = case_when(nutrient == "Vitamin K" & sex=="Females" & age_group=="75-79" ~ "Low overlap",
                          nutrient == "Calcium" & sex=="Females" & age_group=="20-24" ~ "Medium overlap",
                          nutrient == "Magnesium" & sex=="Males" & age_group=="25-29" ~ "High overlap")) %>%
  # Filter
  filter(!is.na(case))

# Simulates
dists_sim <- nutriR::generate_dists(dists) %>%
  # Add case
  left_join(dists %>% select(nutrient, sex, age_group, case))

# Get EARs and percent overlaps
dist_stats <- dists %>%
  # Isolate EAR
  group_by(case, nutrient, sex, age_group) %>%
  summarize(ear=unique(ear)) %>%
  # Add percent overlap
  left_join(stats %>% select(-nutrient_type), by=c("nutrient", "sex", "age_group"="age")) %>%
  mutate(poverlap_label=paste0(round(poverlap, 0), "% overlap")) %>%
  # Add xpos/ypos
  left_join(dists_sim %>% group_by(case) %>% summarize(ypos=max(density[is.finite(density)]), xpos=max(intake))) %>%
  mutate(xpos=case_when(case=="Low overlap" ~ xpos,
                        case=="Medium overlap" ~ xpos,
                        case=="High overlap" ~ xpos))

# Example dists key
dists_ex_key <- dists %>%
  select(case, nutrient_type, nutrient, sex, age_group) %>%
  unique() %>%
  mutate(sex1=recode(sex,
                    "Females"="women",
                    "Males"="men"),
         title=paste0(nutrient, " intake for ", age_group, "-yr-old ", sex1)) %>%
  mutate(label=recode(case,
                      "Low overlap"="B",
                      "Medium overlap"="C",
                      "High overlap"="D")) %>%
  rename(age=age_group)

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
  # Sub-panel points
  geom_text(data=dists_ex_key, mapping=aes(y=nutrient, x=age, label=label), inherit.aes = F, size=2.5) +
  # Labels
  labs(x="Age group (yr)", y="", tag="A") +
  # Legend
  scale_fill_gradientn(name="Median\npercent overlap",
                       colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + main_theme
g1

# Low overlap
title <- dists_ex_key$title[dists_ex_key$case=="Low overlap"]
g2 <- ggplot(dists_sim %>% filter(case=="Low overlap"),
             aes(x=intake, y=density, color=country)) +
  geom_line(lwd=0.5) +
  # Add % overlap label
  annotate(geom="text",
           x=dist_stats$xpos[dist_stats$case=="Low overlap"],
           y=dist_stats$ypos[dist_stats$case=="Low overlap"],
           label=dist_stats$poverlap_label[dist_stats$case=="Low overlap"],
           size=1.8, hjust=1, vjust=1.5) +
  # Add EAR
  geom_vline(xintercept=dist_stats$ear[dist_stats$case=="Low overlap"], linetype="solid", lwd=0.4) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="Low overlap", subtitle=title, tag="B") +
  # Theme
  theme_bw() + subpanel_theme
g2

# Medium overlap
title <- dists_ex_key$title[dists_ex_key$case=="Medium overlap"]
g3 <- ggplot(dists_sim %>% filter(case=="Medium overlap"),
             aes(x=intake, y=density, color=country)) +
  geom_line(lwd=0.5) +
  # Add % overlap label
  annotate(geom="text",
           x=dist_stats$xpos[dist_stats$case=="Medium overlap"],
           y=dist_stats$ypos[dist_stats$case=="Medium overlap"],
           label=dist_stats$poverlap_label[dist_stats$case=="Medium overlap"],
           size=1.8, hjust=1, vjust=1.5) +
  # Add EAR
  geom_vline(xintercept=dist_stats$ear[dist_stats$case=="Medium overlap"], linetype="solid", lwd=0.4) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="Medium overlap", subtitle=title, tag="C") +
  # Theme
  theme_bw() + subpanel_theme
g3

# Merge plots
title <- dists_ex_key$title[dists_ex_key$case=="High overlap"]
g4 <- ggplot(dists_sim %>% filter(case=="High overlap"),
             aes(x=intake, y=density, color=country)) +
  geom_line(lwd=0.5) +
  # lims(x=c(0,750)) +
  # Add % overlap label
  annotate(geom="text",
           x=dist_stats$xpos[dist_stats$case=="High overlap"],
           y=dist_stats$ypos[dist_stats$case=="High overlap"],
           label=dist_stats$poverlap_label[dist_stats$case=="High overlap"],
           size=1.8, hjust=1, vjust=1.5) +
  # Add EAR
  geom_vline(xintercept=dist_stats$ear[dist_stats$case=="High overlap"], linetype="solid", lwd=0.4) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="High overlap", subtitle=title, tag="D") +
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
figtitle <- paste0("Fig3_similarity_across_countries_", ncountries_req, ".png")
ggsave(g, filename=file.path(plotdir, figtitle),
       width=6.5, height=5, units="in", dpi=600)



