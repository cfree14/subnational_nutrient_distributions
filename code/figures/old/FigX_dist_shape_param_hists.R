

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
  # Mark unrepresentative
  mutate(cv_bad=ifelse(cv <= 0.1, "bad", "good"))



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
                     axis.line = element_line(colour = "black"))

# Coefficient of variation
g1 <- ggplot(data, aes(x=cv)) +
  geom_histogram(binwidth = 0.05) +
  # Cutoffs
  geom_vline(xintercept=0.2) +
  # Labels
  labs(x="Coefficient of variation", y="Frequency") +
  # Limits
  scale_x_continuous(lim=c(0, 5), breaks=0:5, labels=c(0:4, "≥5")) +
  # Theme
  theme_bw()
g1

# Skewness
g2 <- ggplot(data, aes(x=skew)) +
  geom_histogram(binwidth = 0.05) +
  # Labels
  labs(x="Skewness", y="Frequency") +
  # Limits
  scale_x_continuous(lim=c(0, 20)) +
  # Theme
  theme_bw()
g2

# Kurtosis
g3 <- ggplot(data, aes(x=kurt)) +
  geom_histogram(binwidth = 0.2) +
  # Labels
  labs(x="Kurtosis", y="Frequency") +
  # Limits
  scale_x_continuous(lim=c(0, 50)) +
  # Theme
  theme_bw()
g3





