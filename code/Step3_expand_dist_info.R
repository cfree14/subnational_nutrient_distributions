

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

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries.Rds"))


# Format EARs
################################################################################

# DRIs
dris <- nutriR::dris

# Get EARs
ears <- dris %>%
  # Reduce to EARs
  filter(grepl("EAR", dri_type) & !is.na(value)) %>%
  # Simplify
  dplyr::select(nutrient, units, sex, age_range, stage, value) %>%
  rename(age_group=age_range, ear_units=units) %>%
  # Spread
  spread(key="stage", value="value") %>%
  # Format EAR
  mutate(ear=ifelse(!is.na(None), None,
                    ifelse(!is.na(Infants), Infants, Children))) %>%
  dplyr::select(-c(None, Infants, Children)) %>%
  rename(ear_lact=Lactation,
         ear_preg=Pregnancy) %>%
  # Arrange
  dplyr::select(nutrient, sex, age_group, ear_units, ear, ear_lact, ear_preg) %>%
  # Recode nutrient names to match intake distributions
  mutate(nutrient=recode(nutrient,
                         "Thiamin"="Thiamine",
                         "Vitamin A"="Vitamin A (RAE)")) %>%
  # Recode EAR units to match intake distributions
  mutate(ear_units=gsub("/d", "", ear_units),
         ear_units=recode(ear_units, "µg"="mcg")) %>%
  # Remove protein EARs which are in different units
  filter(nutrient!="Protein")

# Inspect
ear_nutr <- sort(unique(ears$nutrient))
ear_nutr[!ear_nutr %in% sort(unique(data_orig$nutrient))] # "Molybdenum" "Phosphorus" aren't in our data

ear_units <- sort(unique(ears$ear_units))
ear_units[!ear_units %in% sort(unique(data_orig$nutrient_units))] # "Molybdenum" "Phosphorus" aren't in our data


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Calculate gamma distribution stats
  mutate(g_mu=nutriR::mean_dist(shape=g_shape, rate=g_rate),
         g_cv=nutriR::cv(shape=g_shape, rate=g_rate),
         g_var=nutriR::variance(shape=g_shape, rate=g_rate),
         g_skew=nutriR::skewness(shape=g_shape, rate=g_rate),
         g_kurt=nutriR::kurtosis(shape=g_shape, rate=g_rate)) %>%
  # Calculate log-normal distribution stats
  mutate(ln_mu=nutriR::mean_dist(meanlog = ln_meanlog, sdlog=ln_sdlog),
         ln_cv=nutriR::cv(meanlog = ln_meanlog, sdlog=ln_sdlog),
         ln_var=nutriR::variance(meanlog = ln_meanlog, sdlog=ln_sdlog),
         ln_skew=nutriR::skewness(meanlog = ln_meanlog, sdlog=ln_sdlog),
         ln_kurt=nutriR::kurtosis(meanlog = ln_meanlog, sdlog=ln_sdlog)) %>%
  # Select final values
  mutate(mu=ifelse(best_dist=="gamma", g_mu, ln_mu),
         cv=ifelse(best_dist=="gamma", g_cv, ln_cv),
         var=ifelse(best_dist=="gamma", g_var, ln_var),
         skew=ifelse(best_dist=="gamma", g_skew, ln_skew),
         kurt=ifelse(best_dist=="gamma", g_kurt, ln_kurt)) %>%
  # Add EARs
  mutate(age_group_ear=recode(age_group,
                              "0-4"="1-3 yr",
                              "5-9"="4-8 yr",
                              "10-14"="9-13 yr",
                              "15-19"="14-18 yr",
                              "20-24"="19-30 yr",
                              "25-29"="19-30 yr",
                              "30-34"="31-50 yr",
                              "35-39"="31-50 yr",
                              "40-44"="31-50 yr",
                              "45-49"="31-50 yr",
                              "50-54"="51-70 yr",
                              "55-59"="51-70 yr",
                              "60-64"="51-70 yr",
                              "65-69"="51-70 yr",
                              "70-74"=">70 yr",
                              "75-79"=">70 yr",
                              "80-84"=">70 yr",
                              "85-89"=">70 yr",
                              "90-94"=">70 yr",
                              "95-99"=">70 yr"),
         sex_ear=ifelse(age_group_ear %in% c("1-3 yr", "4-8 yr"), "Both", sex)) %>%
  left_join(ears, by=c("nutrient"="nutrient", "age_group_ear"="age_group", "sex_ear"="sex")) %>%
  # Arrange
  dplyr::select(country:age_group,
                sex_ear, age_group_ear,
                ear_units, ear, ear_lact, ear_preg,
                g_shape:g_ks, g_mu:g_kurt, ln_meanlog:ln_ks, ln_mu:ln_kurt, best_dist, everything())

# Inspect
freeR::complete(data)
sum(data$nutrient_units!=data$ear_units, na.rm=T)


# Export data
################################################################################

# Export
saveRDS(data, file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds"))
write.csv(data, file=file.path(datadir, "nutrient_intake_distributions_22countries_expanded.csv"), row.names=F)



# Plot data
################################################################################

plot(ln_cv ~ g_cv, data, ylim=c(0,3), xlim=c(0,3))
abline(a=0, b=1)

g <- ggplot(data, aes(x=pmin(g_cv,3), y=pmin(ln_cv,6), col=best_dist)) +
  geom_point()
g

# Skewness by nutrient
g <- ggplot(data, aes(y=nutrient, x=pmin(skew, 5))) +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  geom_boxplot() +
  # Labels
  labs(x="Skewness", y="") +
  # Theme
  theme_bw()
g

# Kurtosis by nutrient
g <- ggplot(data, aes(y=nutrient, x=pmin(kurt, 70))) +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  geom_boxplot() +
  # Labels
  labs(x="Kurtosis", y="") +
  # Theme
  theme_bw()
g

# Skewness ~ kurtosis
g <- ggplot(data, aes(x=pmin(skew, 5), y=pmin(kurt, 70), color=best_dist)) +
  geom_point()
g



# By age?
data_vit <- data %>% filter(nutrient_type=="Vitamin" & sex!="Children") %>%
  mutate(group=paste(country, nutrient, sex, sep="-"))
g <- ggplot(data_vit, aes(x=age_group, y=pmin(skew, 5), color=country, linetype=sex, group=group)) +
  facet_wrap(~nutrient) +
  geom_line() +
  theme_bw()
g


g <- ggplot(data_vit, aes(y=country, x=pmin(skew, 5)))  +
  facet_wrap(~nutrient) +
  geom_boxplot() +
  theme_bw()
g



