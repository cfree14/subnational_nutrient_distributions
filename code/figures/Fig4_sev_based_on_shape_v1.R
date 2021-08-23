

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds"))

# Format data
data <- data_orig %>%
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  filter(sex!="Children") %>%
  # Fix SEV from cutpoint
  mutate(cutpoint_sev=100*cutpoint_sev) %>%
  # Calculate difference
  mutate(sev_diff=cutpoint_sev-sev,
         meandivear=mu/ear)

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

# SEV based on variability
g1 <- ggplot(data, aes(x=pmin(mu/ear, 5), y=sev, fill=pmin(cv,1))) +
  geom_point(pch=21, size=1, stroke=0.1) +
  # Reference line
  geom_vline(xintercept=1, linetype="dashed") +
  # Reference curve
  geom_line(data=ref_df, mapping=aes(x=scalar, y=sev), inherit.aes = F) +
  # Labels
  labs(x="Mean intake /\nmean requirement", # add extra space to align with below
       y="% with inadequate intake\n(summary exposure value)",
       tag="A") +
  # Legend
  scale_fill_gradientn(name="Coefficient\nof variation",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0,1, 0.25),
                       labels=c(seq(0, 0.75, 0.25), "≥1.0")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axes
  scale_x_continuous(breaks=0:5, labels=c(0:4, "≥5")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.85,0.8),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)))
g1

# SEV probability method compared to cutpoint method
g2 <- ggplot(data, aes(y=cutpoint_sev, x=sev, fill=pmin(cv,1))) +
  geom_point(pch=21, size=1, stroke=0.1) +
  # Horizontal line
  geom_abline(slope=1) +
  # Label sides
  annotate(geom="text", x=100, y=5, hjust=1, label="Cutpoint method underestimates\nprevalence of inadequate intakes", size=2.2) +
  annotate(geom="text", x=10, y=95, hjust=0, label="Cutpoint method overestimates\nprevalence of inadequate intakes", size=2.2) +
  # Labels
  labs(y="% inaqequate intake (SEV)\nestimated using the cutpoint method",
       x="% inadequate intake (SEV)\nestimated using the probability method",
       tag="B") +
  # Legend
  scale_fill_gradientn(name="Coefficient\nof variation",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0,1, 0.25),
                       labels=c(seq(0,0.75, 0.25), "≥1.0")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)


# Export
ggsave(g, filename=file.path(plotdir, "Fig4_sev_based_on shape.png"),
       width=6.5, height=3, units="in", dpi=600)

