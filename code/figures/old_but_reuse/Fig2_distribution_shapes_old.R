

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
  filter(sex!="Children")



# Build data
################################################################################

# Set sort order
key <- data_orig %>%
  group_by(nutrient_type, nutrient) %>%
  summarize(cv_med=median(cv, na.rm=T)) %>%
  arrange(nutrient_type, cv_med)

# Build data
data <- data_orig %>%
  # Simplify
  dplyr::select(country:age_group, best_dist, cv, skew, kurt) %>%
  gather(key="metric", value="value", 9:11) %>%
  # Recod
  mutate(metric=recode_factor(metric, "cv"="Coefficient of variation (CV)", "skew"="Skewness", "kurt"="Excess kurtosis")) %>%
  # Cap
  mutate(cap=recode(metric,
                    "Coefficient of variation (CV)"="4",
                    "Skewness"="5",
                    "Excess kurtosis"="30") %>% as.character() %>% as.numeric(),
         value_cap=pmin(value, cap)) %>%
  # Set order
  mutate(nutrient=factor(nutrient, levels=key$nutrient))

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


# Plot data
g <- ggplot(data, aes(x=value_cap, y=nutrient, fill=sex, alpha=0.2)) +
  facet_grid(nutrient_type ~ metric, scales="free", space="free_y") +
  geom_boxplot(outlier.size=0.5, lwd=0.2, position = "identity") +
  # Labels
  labs(x="Metric of distribution shape", y="") +
  scale_fill_discrete(name="") +
  guides(alpha="none") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_dist_shapes_by_nutr_gender.png"),
       width=6.5, height=5, units="in", dpi=600)




