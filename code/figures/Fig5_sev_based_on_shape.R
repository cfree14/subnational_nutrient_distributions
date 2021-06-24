

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
data <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds")) %>%
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  filter(sex!="Children")

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
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=6),
                     plot.title=element_text(size=7),
                     plot.tag=element_text(size=7),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom",
                     legend.key.size = unit(0.3, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# SEV based on variability
g1 <- ggplot(data, aes(x=pmin(mu/ear, 5), y=sev, fill=pmin(cv,1))) +
  geom_point(pch=21, size=1, stroke=0.1) +
  # Reference line
  geom_vline(xintercept=1, linetype="dashed") +
  # Reference curve
  geom_line(data=ref_df, mapping=aes(x=scalar, y=sev), inherit.aes = F) +
  # Labels
  labs(x="Mean intake / mean requirement", y="% with inadequate intake\n(summary exposure value)",
       title="Health outcomes\nbased on intake variability", tag="A") +
  # Legend
  scale_fill_gradientn(name="Coefficient\nof variation",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0,1, 0.25),
                       labels=c(seq(0,0.75, 0.25), "≥1.0")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axes
  scale_x_continuous(breaks=0:5, labels=c(0:4, "≥5")) +
  # Theme
  theme_bw() + base_theme
g1

# SEV based on skewness
g2 <- ggplot(data, aes(x=pmin(mu/ear, 5), y=sev, fill=pmin(skew,3))) +
  geom_point(pch=21, size=1, stroke=0.1) +
  # Reference line
  geom_vline(xintercept=1, linetype="dashed") +
  # Reference curve
  geom_line(data=ref_df, mapping=aes(x=scalar, y=sev), inherit.aes = F) +
  # Labels
  labs(x="Mean intake / mean requirement", y="% with inadequate intake\n(summary exposure value)",
       title="Health outcomes\nbased on intake asymmetry", tag="B") +
  # Legend
  scale_fill_gradientn(name="Skewness\n ",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=0:3,
                       labels=c(0:2, "≥3")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axes
  scale_x_continuous(breaks=0:5, labels=c(0:4, "≥5")) +
  # Theme
  theme_bw() + base_theme
g2

# SEV based on kurtosis
g3 <- ggplot(data, aes(x=pmin(mu/ear, 5), y=sev, fill=pmin(kurt,10))) +
  geom_point(pch=21, size=1, stroke=0.1) +
  # Reference line
  geom_vline(xintercept=1, linetype="dashed") +
  # Reference curve
  geom_line(data=ref_df, mapping=aes(x=scalar, y=sev), inherit.aes = F) +
  # Labels
  labs(x="Mean intake / mean requirement", y="% with inadequate intake\n(summary exposure value)",
       title="Health outcomes\nbased on intake tailedness", tag="C") +
  # Legend
  scale_fill_gradientn(name="Excess\nkurtosis",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0, 10, 2),
                       labels=c(seq(0,8,2), "≥10")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Axes
  scale_x_continuous(breaks=0:5, labels=c(0:4, "≥5")) +
  # Theme
  theme_bw() + base_theme
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_sev_based_on shape.png"),
       width=6.5, height=3, units="in", dpi=600)
