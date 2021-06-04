

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read file key
data <- read.csv(file=file.path(datadir, "SPADE_file_key.csv"), as.is=T)


# Build data
################################################################################

# Build table
stats <- data %>%
  # Stats
  group_by(type1, type2, nutrient, continent, country) %>%
  summarize(sexes=paste(sort(unique(sex)), collapse="-")) %>%
  ungroup() %>%
  # Recode
  mutate(sexes=recode_factor(sexes,
                            "Males"="Males only",
                            "Females"="Females only",
                            "Females-Males"="Both males and females",
                            "Children-Females-Males"="Both males and females")) %>%
  mutate(type2=recode(type2,
                      "Other macronutrient"="Other\nmacronutrient"))


# Plot data
################################################################################

# Plot
g <- ggplot(stats, aes(x=country, y=nutrient, fill=sexes)) +
  facet_grid(type2~continent, scales="free", space="free") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_ordinal(name="Sexes with data") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=7),
        strip.text=element_text(size=6),
        plot.title=element_blank(),
        legend.position = "top")
g

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_data_coverage_broad.png"),
       width=6.5, height=6.5, units="in", dpi=600)

