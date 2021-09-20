

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
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded_final.Rds")) %>%
  filter(best_dist!="none" & status!="Not representative")


# Build data
################################################################################

# Number of countries required to be included
ncountries_req <- 3

# Set sort order
key <- data_orig %>%
  # Remove children
  filter(sex!="Children") %>%
  # Sample size and variability
  group_by(nutrient_type, nutrient) %>%
  summarize(n=n(),
            ncountries=n_distinct(country),
            cv_med=median(cv, na.rm=T)) %>%
  ungroup() %>%
  # Arrange by variability
  arrange(nutrient_type, cv_med) %>%
  # Eliminate low sample size
  filter(ncountries>=ncountries_req)

# Build data
data <- data_orig %>%
  # Remove children
  filter(sex!="Children") %>%
  # Reduce to nutrients meeting data requirements
  filter(nutrient %in% key$nutrient) %>%
  # Simplify
  dplyr::select(country:age_group, best_dist, cv, skew) %>%
  # Gather
  gather(key="metric", value="value", 11:12) %>%
  # Aggregate some nutrient types
  mutate(nutrient_type=ifelse(nutrient %in% c("Sugar", "beta-Carotene", "alpha-Carotene", "beta-cryptoxanthin"),
                              "Other macronutrient", nutrient_type)) %>%
  # Recode nutrient types
  mutate(nutrient_type=recode(nutrient_type, "Other macronutrient"="Other\nmacronutrient")) %>%
  # Recode metric names
  mutate(metric=recode_factor(metric, "cv"="Coefficient of variation (CV)", "skew"="Skewness")) %>%
  # Cap
  mutate(cap=recode(metric,
                    "Coefficient of variation (CV)"="2",
                    "Skewness"="5") %>% as.character() %>% as.numeric(),
         value_cap=pmin(value, cap)) %>%
  # Set order
  mutate(nutrient=factor(nutrient, levels=key$nutrient))


# Build distribution examples
################################################################################

# Calculate density at mean
calc_dens_at_mean <- function(best_dist, mu, shape, rate, meanlog, sdlog){

  # If gamma
  if(best_dist=="gamma"){
    dens_at_mean <- dgamma(x=mu, shape=shape, rate=rate)
  }else{
    dens_at_mean <- dlnorm(x=mu, meanlog=meanlog, sdlog=sdlog)
  }
  dens_at_mean
}

# CV examples
###################

# Identify CV examples
dists_cv <- data_orig %>%
  # Filter
  filter(nutrient=="Calcium" & sex=="Females" & age_group=="20-24") %>%
  filter(country %in% c("Burkina Faso", "United States", "Canada")) %>%
  # Add density at mean
  rowwise() %>%
  mutate(dens_at_mean=calc_dens_at_mean(best_dist, mu, g_shape, g_rate, ln_meanlog, ln_sdlog)) %>%
  ungroup() %>%
  # Add country label
  mutate(country_label=paste0(country, " (", round(sev, 0), "%)"))

# Generate distributions to plot
dist_cv_ear <- dists_cv$ear %>% unique()
dist_cv_sim <- nutriR::generate_dists(dists_cv) %>%
  # Add country label
  left_join(dists_cv %>% select(country, country_label))

# Plot example distributions
ggplot(dist_cv_sim, aes(x=intake, y=density, color=country_label)) +
  geom_line() +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="Calcium intake for 20-24-yr-old women") +
  # Theme
  theme_bw()

# Skew examples
###################

# Identify skewness examples
dists_skew <- data_orig %>%
  # Filter
  filter(nutrient=="Vitamin A (RAE)" & sex=="Males" & age_group=="45-49") %>%
  filter(country %in% c("Portugal", "Netherlands", "Italy")) %>%
  # Add density at mean
  rowwise() %>%
  mutate(dens_at_mean=calc_dens_at_mean(best_dist, mu, g_shape, g_rate, ln_meanlog, ln_sdlog)) %>%
  ungroup() %>%
  # Add country label
  mutate(country_label=paste0(country, " (", round(sev, 0), "%)"))

# Generate distributions to plot
dist_skew_ear <- dists_skew$ear %>% unique()
dist_skew_sim <- nutriR::generate_dists(dists_skew) %>%
  left_join(dists_skew %>% select(country, country_label))

# Plot example distributions
ggplot(dist_skew_sim, aes(x=intake, y=density, color=country)) +
  geom_line() +
  # Labels
  labs(x="Habitual intake (mg)", y="Density",
       title="Vitamin A intake for 45-49-yr-old men") +
  # Theme
  theme_bw()


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=4),
                     legend.title=element_text(size=5),
                     strip.text=element_text(size=6),
                     plot.title=element_text(size=7),
                     plot.subtitle=element_text(size=6),
                     plot.tag=element_text(size=8, face="bold"),
                     # Gridlines
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(colour = "black"))


# Plot data
g1 <- ggplot(data, aes(x=value_cap, y=nutrient, fill=sex, alpha=0.2)) +
  facet_grid(nutrient_type ~ metric, scales="free", space="free_y") +
  geom_boxplot(outlier.size=0.5, lwd=0.2, position = "identity") +
  # Labels
  labs(x="Metric of distribution shape", y="", tag="A",
       title="Distribution shape varies by nutrient and sex") +
  scale_fill_discrete(name="") +
  guides(alpha="none") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "bottom")
g1

# Plot CV examples
legend.x <- max(dist_cv_sim$density)
legend.y <- max(dist_cv_sim$intake)
g2 <- ggplot(dist_cv_sim, aes(x=intake, y=density, color=country_label)) +
  # Intakes
  geom_line(lwd=0.3) +
  # Plot means
  geom_point(data=dists_cv, mapping=aes(x=mu, y=dens_at_mean, color=country_label),
             inherit.aes = F, show.legend=F, size=0.6) +
  geom_segment(data=dists_cv, mapping=aes(x=mu, xend=mu, y=0, yend=dens_at_mean, color=country_label),
               linetype="dotted", lwd=0.3, inherit.aes = F, show.legend=F) +
  # EAR
  geom_vline(xintercept=dist_cv_ear, linetype="solid", lwd=0.3) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density", tag="B",
       title="Impact of variability",
       subtitle="Calcium intake for 20-24-yr-old women") +
  # Legend
  scale_color_discrete(name="Country\n(% inadequate intake)") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.7, 0.8),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Plot skewness examples
g3 <- ggplot(dist_skew_sim, aes(x=intake, y=density, color=country_label)) +
  # Intakes
  geom_line(lwd=0.3) +
  # Plot means
  geom_point(data=dists_skew, mapping=aes(x=mu, y=dens_at_mean, color=country_label),
             inherit.aes = F, show.legend=F, size=0.6) +
  geom_segment(data=dists_skew, mapping=aes(x=mu, xend=mu, y=0, yend=dens_at_mean, color=country_label),
               linetype="dotted", lwd=0.3, inherit.aes = F, show.legend=F) +
  # EAR
  geom_vline(xintercept=dist_skew_ear, linetype="solid", lwd=0.3) +
  # Labels
  labs(x="Habitual intake (mg)", y="Density", tag="C",
       title="Impact of skewness",
       subtitle="Vitamin A intake for 40-45-yr-old men") +
  # Legend
  scale_color_discrete(name="Country\n(% inadequate intake)") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.7, 0.8),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill=alpha('blue', 0)),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3,
                             layout_matrix=matrix(data=c(1,2,
                                                  1,3), ncol=2, byrow=T),
                             widths=c(0.7, 0.3))


# Export
ggsave(g, filename=file.path(plotdir, "Fig2_distribution_shapes.png"),
       width=6.5, height=5, units="in", dpi=600)




