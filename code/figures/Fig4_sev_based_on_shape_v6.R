

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

# Format data
data <- data_orig %>%
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  filter(sex!="Children") %>%
  # Calculate difference
  mutate(sev_diff=cutpoint_sev-sev,
         meandivear=mu/ear_use,
         meandivear_cap=pmin(5, meandivear)) %>%
  # Add some categories
  mutate(meandivear_catg=ifelse(meandivear>=1, "above", "below"))

# Build data for raster
data2 <- data_orig %>%
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  # mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  filter(sex!="Children") %>%
  # FIlter to EAR/SEV
  filter(!is.na(sev)) %>%
  # Mean SEV by country, nutrient, sex (so across age groups)
  group_by(nutrient, country, iso3, representativeness, sex) %>%
  summarize(sev=mean(sev)) %>%
  ungroup() %>%
  # Remove Vitamin D
  filter(nutrient!="Vitamin D")

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


# Build reference data
################################################################################

# Cutpoint
cutpoint <- function(intake_avg, intake_cv, ear){

  # Calculate p(deficient)
  pdeficient <- pnorm(q=ear, mean=intake_avg, sd=intake_avg*intake_cv)

  # Return
  return(pdeficient)

}

# Build cutpoint
ref_df <- tibble(ear=1000,
                 scalar=seq(0, 5, 0.01),
                 intake_avg=ear*scalar,
                 intake_cv=0.25,
                 sev=cutpoint(intake_avg, intake_cv, ear)*100)

# Color key
color_key <- tibble()


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

# SEV raster
g1 <- ggplot(data2_ordered, aes(x=iso3, y=nutrient, fill=sev)) +
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
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# SEV based on variability
g2 <- ggplot(data, aes(x=pmin(meandivear, 5), y=sev, fill=pmin(cv,1))) +
  geom_point(pch=21, size=1, stroke=0.1) +
  # Reference line
  geom_vline(xintercept=1, linetype="dashed") +
  # Reference curve
  geom_line(data=ref_df, mapping=aes(x=scalar, y=sev), inherit.aes = F) +
  # Labels
  labs(x="Mean intake /\nmean requirement", # add extra space to align with below
       y="% inadequate intake\n(probability approach)",
       tag="B") +
  # Legend
  scale_fill_gradientn(name="Coefficient\nof variation",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0,1, 0.25),
                       labels=c(seq(0, 0.75, 0.25), "≥1.0"),
                       lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axes
  scale_x_continuous(breaks=0:5, labels=c(0:4, "≥5")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85, 0.75),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g2

# SEV probability method compared to cutpoint method (assuming normal)
g3 <- ggplot(data, aes(x=sev, y=cutpoint_sev_norm, fill=pmin(skew,3))) + #, size=meandivear_cap)) +
  geom_point(pch=21, stroke=0.1, size=1) + # size=1
  # Horizontal line
  geom_abline(slope=1) +
  # Label sides
  # annotate(geom="text", x=100, y=5, hjust=1, label="Cut-point method underestimates\nprevalence of inadequate intakes", size=2.2) +
  # annotate(geom="text", x=0, y=95, hjust=0, label="Cut-point method overestimates\nprevalence of inadequate intakes", size=2.2) +
  # Labels
  labs(x="% inadequate intake\n(probability approach)",
       y="% inaqequate intake\n(cut-point method\nassuming a normal distribution)",
       tag="C") +
  # Legend
  scale_fill_gradientn(name=" \nSkewness",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0, 3, 1),
                       labels=c(seq(0,2,1), "≥3"),
                       lim=c(0,3)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # scale_size_continuous(name="Mean/EAR", breaks = seq(0.5, 1, 2, 5))
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.15, 0.75),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g3

# SEV probability method compared to cutpoint method (assuming normal)
g4 <- ggplot(data, aes(x=sev, y=cutpoint_sev, fill=pmin(skew,3))) + #, size=meandivear_cap)) +
  geom_point(pch=21, stroke=0.1, size=1) + # size=1
  # Horizontal line
  geom_abline(slope=1) +
  # Label sides
  # annotate(geom="text", x=100, y=5, hjust=1, label="Cut-point method underestimates\nprevalence of inadequate intakes", size=2.2) +
  # annotate(geom="text", x=0, y=95, hjust=0, label="Cut-point method overestimates\nprevalence of inadequate intakes", size=2.2) +
  # Labels
  labs(x="% inadequate intake\n(probability approach)",
       y="% inaqequate intake\n(cut-point method\nassuming the correct distribution)",
       tag="D") +
  # Legend
  scale_fill_gradientn(name=" \nSkewness",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0, 3, 1),
                       labels=c(seq(0,2,1), "≥3"),
                       lim=c(0,3)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # scale_size_continuous(name="Mean/EAR", breaks = seq(0.5, 1, 2, 5))
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.15, 0.75),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g4

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4,
                             layout_matrix=matrix(data=c(1,1,1,
                                                         2,3,4), nrow=2, byrow=T), heights=c(0.55, 0.45))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_sev_based_on shape_v6.png"),
       width=6.5, height=5, units="in", dpi=600)

