

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
tabledir <- "tables"

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded.Rds"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  filter(!is.na(sev_ear) & !is.na(sev_ar))

# AR key
nrvs <- nutriR::nrvs
ar_key <- nrvs %>%
  # Reduce to ARs
  filter(nrv_type=="Average requirement" & !is.na(nrv)) %>%
  # Source for unique nutrients
  mutate(nutrient=ifelse(grepl("Iron", nutrient), "Iron", nutrient),
         nutrient=ifelse(grepl("Zinc", nutrient), "Zinc", nutrient),
         nutrient=recode(nutrient,
                         "Vitamin B-6"="Vitamin B6",
                         "Vitamin B-12"="Vitamin B12",
                         "Thiamin"="Thiamine",
                         "Vitamin A"="Vitamin A (RAE)")) %>%
  select(nutrient, units, source) %>%
  unique() %>%
  # Reduce to ones with SEVs
  filter(nutrient %in% data$nutrient)

# Plot data
################################################################################

# My theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.title=element_text(size=8),
                  strip.text=element_text(size=8),
                  plot.title=element_text(size=10),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"))

# Are SEVs with ARs and EARs related?
g <- ggplot(data, aes(x=sev_ear, y=sev_ar)) +
  facet_wrap(~nutrient, ncol=6) +
  # Plot points
  geom_point(pch=21, alpha=0.5, fill="grey80", stroke=0.2) +
  # 1:1 Line
  geom_abline(slope=1) +
  # Plot text
  geom_text(data=ar_key, mapping=aes(color=source, label=source),
            x=100, y=0, hjust=1, vjust=0, size=2, show.legend = F) +
  # Labels
  labs(x="% inadequate intake\nwhen using EAR values",
       y="% inadequate intake\nwhen using AR values") +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_sevs_ears_vs_ars.png"),
       width=6.5, height=4.75, units="in", dpi=600)




