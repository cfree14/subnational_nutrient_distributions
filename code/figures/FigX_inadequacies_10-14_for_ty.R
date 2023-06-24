

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_31countries_expanded_final.Rds")) %>%
  filter(best_dist!="none" & status!="Not representative")


# Build data
################################################################################

# Build data for raster
data2 <- data_orig %>%
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  # mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  filter(sex!="Children") %>%
  filter(age_group=="10-14") %>%
  # FIlter to EAR/SEV
  filter(!is.na(sev)) %>%
  # Mean SEV by country, nutrient, sex (so across age groups)
  group_by(nutrient, country, iso3, representativeness, sex) %>%
  summarize(sev=mean(sev)) %>%
  ungroup() %>%
  # Remove Vitamin D and carbohydrates
  filter(!nutrient %in% c("Vitamin D", "Carbohydrates"))

cntry_key <- data2 %>%
  group_by(iso3, country, representativeness) %>%
  summarize(sev=mean(sev)) %>%
  arrange(sev)

nutrient_key <- data2 %>%
  group_by(nutrient) %>%
  summarize(sev=mean(sev)) %>%
  arrange(sev)

# Order data
data2_ordered <- data2 %>%
  mutate(iso3=factor(iso3, levels=cntry_key$iso3),
         nutrient=factor(nutrient, levels=nutrient_key$nutrient)) %>%
  # Add percentage labels
  mutate(perc_label=paste0(round(sev), "")) %>%
  # Add color label
  mutate(text_color=ifelse(sev>=50, "white", "black"))

# Color key
label_key <- data2_ordered %>%
  select(sex, iso3, representativeness) %>%
  unique() %>%
  arrange(sex, iso3) %>%
  mutate(color=recode(representativeness, "Local"="red", "Regional"="blue", "National"="black"))



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
                     axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(data2_ordered, aes(x=iso3, y=nutrient, fill=sev)) +
  facet_grid(.~sex, scales="free_x", space="free_x") +
  # Plot raster
  geom_tile() +
  # geom_tile(color="grey40", lwd=0.1) +
  # Plot text
  geom_text(data=data2_ordered, aes(x=iso3, y=nutrient, label=perc_label, color=text_color),
            size=1.6, inherit.aes = F, show.legend = F) +
  # Labels
  labs(x="", y="", tag="A") +
  # Text legend
  # scale_color_gradient2(name="", midpoint=50,
  #                       low="black", high="white", lim=c(0,100)) +
  scale_color_manual(name="", values=c("black", "white")) +
  # Fill legend
  scale_fill_gradientn(name="Mean %\ninadequate intake\n(probability approach)",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       lim=c(0,100), na.value = "grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(plot.title=element_blank(),
        axis.title=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "right",
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(-5,-5,-5,-5))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_inadequacies_10-14_for_ty.png"),
       width=6.5, height=5, units="in", dpi=600)









