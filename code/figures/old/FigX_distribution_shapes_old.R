

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

# C.V. of nutrient intakes
################################################################################

# Set sort order
key1 <- data %>%
  group_by(nutrient_type, nutrient) %>%
  summarize(cv_med=median(cv, na.rm=T)) %>%
  arrange(nutrient_type, cv_med)
data1 <- data %>%
  mutate(nutrient=factor(nutrient, levels=key1$nutrient))


# Coefficient of variation
g1 <- ggplot(data1, aes(y=nutrient, x=pmin(cv, 4), fill=nutrient_type)) +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="Coefficient of variation (CV)", y="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigX_intake_variability_by_nutrient.png"),
       width=6.5, height=5, units="in", dpi=600)

# C.V. of nutrient intakes by sex
################################################################################

# Set sort order
key1 <- data %>%
  group_by(nutrient_type, nutrient) %>%
  summarize(cv_med=median(cv, na.rm=T)) %>%
  arrange(nutrient_type, cv_med)
data1 <- data %>%
  mutate(nutrient=factor(nutrient, levels=key1$nutrient))


# Coefficient of variation
g1 <- ggplot(data1, aes(y=nutrient, x=pmin(cv, 4), fill=sex, alpha=0.2)) +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  geom_boxplot(outlier.size=0.5, lwd=0.2, position = "identity") +
  # Labels
  labs(x="Coefficient of variation (CV)", y="") +
  scale_fill_discrete(name="") +
  guides(alpha=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g1

# Export
ggsave(g1, filename=file.path(plotdir, "FigX_intake_variability_by_gender.png"),
       width=6.5, height=5, units="in", dpi=600)


# Skewness of nutrient intakes
################################################################################

# Set sort order
key2 <- data %>%
  group_by(nutrient_type, nutrient) %>%
  summarize(skew_med=median(skew, na.rm=T)) %>%
  arrange(nutrient_type, skew_med)

# Build data
data2 <- data %>%
  # Simplify
  dplyr::select(country:age_group, best_dist, skew, kurt) %>%
  gather(key="metric", value="value", 9:10) %>%
  # Recod
  mutate(metric=recode_factor(metric, "skew"="Skewness", "kurt"="Excess kurtosis")) %>%
  # Cap
  mutate(cap=ifelse(metric=="Skewness", 5, 30),
         value_cap=pmin(value, cap)) %>%
  # Set order
  mutate(nutrient=factor(nutrient, levels=key2$nutrient))

# Plot data
g2 <- ggplot(data2, aes(x=value_cap, y=nutrient, fill=nutrient_type)) +
  facet_grid(nutrient_type ~ metric, scales="free", space="free_y") +
  geom_boxplot(outlier.size=0.1, lwd=0.2) +
  # Labels
  labs(x="Measure of asymmetry", y="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigX_intake_skewness_kurtosis_by_nutrient.png"),
       width=6.5, height=5, units="in", dpi=600)



# Skewness of nutrient intakes bu gender
################################################################################

# Set sort order
key2 <- data %>%
  group_by(nutrient_type, nutrient) %>%
  summarize(skew_med=median(skew, na.rm=T)) %>%
  arrange(nutrient_type, skew_med)

# Build data
data2 <- data %>%
  # Simplify
  dplyr::select(country:age_group, best_dist, skew, kurt) %>%
  gather(key="metric", value="value", 9:10) %>%
  # Recod
  mutate(metric=recode_factor(metric, "skew"="Skewness", "kurt"="Excess kurtosis")) %>%
  # Cap
  mutate(cap=ifelse(metric=="Skewness", 5, 30),
         value_cap=pmin(value, cap)) %>%
  # Set order
  mutate(nutrient=factor(nutrient, levels=key2$nutrient))

# Plot data
g2 <- ggplot(data2, aes(x=value_cap, y=nutrient, fill=sex, alpha=0.2)) +
  facet_grid(nutrient_type ~ metric, scales="free", space="free_y") +
  geom_boxplot(outlier.size=0.5, lwd=0.2, position = "identity") +
  # Labels
  labs(x="Measure of asymmetry", y="") +
  scale_fill_discrete(name="") +
  guides(alpha=F) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g2

# Export
ggsave(g2, filename=file.path(plotdir, "FigX_intake_skewness_kurtosis_by_nutrient_by_gender.png"),
       width=6.5, height=5, units="in", dpi=600)




