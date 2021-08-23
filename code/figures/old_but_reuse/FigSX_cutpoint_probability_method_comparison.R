

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds")) %>%
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  mutate(sex=recode(sex, "Males"="Men", "Females"="Women")) %>%
  filter(sex!="Children") %>%
  # Fix SEV from cutpoint
  mutate(cutpoint_sev=100*cutpoint_sev) %>%
  # Calculate difference
  mutate(sev_diff=cutpoint_sev-sev,
         meandivear=mu/ear)


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
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Scatterplot
g1 <- ggplot(data_orig, aes(y=cutpoint_sev, x=sev, fill=pmin(skew,3))) +
  geom_point(pch=21) +
  # Horizontal line
  geom_abline(slope=1) +
  # Label sides
  annotate(geom="text", x=100, y=10, hjust=1, label="Cutpoint method underestimates\nprevalence of inadequate intakes", size=2.5) +
  annotate(geom="text", x=10, y=100, hjust=0, label="Cutpoint method overestimates\nprevalence of inadequate intakes", size=2.5) +
  # Labels
  labs(y="% inaqequate intake (SEV)\nestimated using the cutpoint method",
       x="% inadequate intake (SEV)\nestimated using the probability method") +
  # Legend
  scale_fill_gradientn(name="Skewness",
                       colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       breaks=seq(0, 3, 1),
                       labels=c(seq(0,2,1), "≥3")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g1

# Histogram
g2 <- ggplot(data_orig, aes(x=sev_diff)) +
  geom_histogram(breaks=seq(-50,100,2.5)) +
  geom_vline(xintercept=0, lwd=0.4) +
  # Label sides
  annotate(geom="text", x=-50, y=2800, hjust=0, vjust=1, label="Cutpoint\nmethod\nunderestimates\nprevalence", size=1.8) +
  annotate(geom="text", x=100, y=2800, hjust=1, vjust=1, label="Cutpoint\nmethod\noverestimates\nprevalence", size=1.8) +
  # Labels
  labs(x="Difference in % inadequate intakes\nestimated by the cutpoint and probability methods", y="Number of\ncountry-nutrient-sex-age groups") +
  # Theme
  theme_bw() + base_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, layout_matrix=matrix(data=c(1,2,
                                                                 1,3), ncol=2, byrow = T), widths=c(0.65, 0.35))


# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_cutpoint_probability_method.png"),
       width=6.5, height=4.5, units="in", dpi=600)



