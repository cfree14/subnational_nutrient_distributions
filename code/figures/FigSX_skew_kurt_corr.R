

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
  filter(best_dist!="none")



# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=5),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=6),
                     plot.title=element_blank(),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.position = c(0.2, 0.8))

# Correlation
g <- ggplot(data, aes(x=skew, y=kurt, color=best_dist)) +
  geom_point() +
  # Labels
  labs(x="Skewness", y="Excess kurtosis") +
  # Limits
  scale_x_continuous(lim=c(0, 5), breaks=0:5, labels=c(0:4, "≥5")) +
  scale_y_continuous(lim=c(0, 70), breaks=seq(0, 70, 5), labels=c(seq(0, 65, 5), "≥70")) +
  # Legend
  scale_color_discrete(name="Distribution type") +
  # Theme
  theme_bw() + base_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_skew_kurt_corr.png"),
       width=3.5, height=3.5, units="in", dpi=600)




