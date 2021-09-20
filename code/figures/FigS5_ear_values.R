

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read data
dists <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded_final.Rds"))
nutrients <- sort(unique(dists$nutrient))

# DRIs
dris <- nutriR::dris
nrvs <- nutriR::nrvs
sort(unique(nrvs$nutrient))

# Build data
################################################################################

# Build ARs
ars <- nrvs %>%
  # ARs
  filter(nrv_type=="Average requirement") %>%
  # Nutrients of interest
  mutate(nutrient=recode(nutrient,
                         "Vitamin B-12"="Vitamin B12",
                         "Vitamin B-6"="Vitamin B6",
                         "Vitamin A"="Vitamin A (RAE)",
                         "Thiamin"="Thiamine")) %>%
  filter(nutrient!="Protein") %>%
  filter(nutrient %in% nutrients | grepl("Iron", nutrient) | grepl("Zinc", nutrient)) %>%
  # Rename
  rename(value=nrv, type=nrv_type) %>%
  # Format
  mutate(age_group=gsub(" y", " yr", age_group)) %>%
  # Arrange
  select(source, type, nutrient, units, source, sex, stage, age_group, value)

# Build EARs
ears <- dris %>%
  # Reduce to carbohydrate EARs
  filter(nutrient=="Carbohydrates" & dri_type=="Estimated Average Requirement (EAR)") %>%
  # Add/rename
  mutate(source="IOM") %>%
  rename(type=dri_type, age_group=age_range) %>%
  # Arrange
  select(source, type, nutrient, units, source, sex, stage, age_group, value)

# Merge requirements
reqs <- bind_rows(ars, ears)


# Build age key
age_key <- reqs %>%
  # Unique ages
  select(age_group) %>%
  unique() %>%
  # Recode
  mutate(age_group_simple=recode(age_group,
                                 "0-6 mo"="0-0.5 yr",
                                 "6-12 mo"="0.5-1 yr",
                                 "7-11 mo"="0.5-1 yr",
                                 ">70 yr"="71-100 yr",
                                 ">70 y"="71-100 yr",
                                 "≤ 18 y"="0-18 yr",
                                 "≤ 18 yr"="0-18 yr") %>% gsub(" yr", "", .)) %>%
  # Seperate
  tidyr::separate(age_group_simple, sep="-", into=c("age1", "age2"), remove=F) %>%
  # Gather
  select(age_group, age_group_simple, age1, age2) %>%
  gather(key="endpoint", value='age_yr', 3:4) %>%
  mutate(endpoint=ifelse(endpoint=="age1", 1, 2),
         age_yr=age_yr %>% as.numeric()) %>%
  arrange(age_group, endpoint)

# Expand EARs
reqs_expanded <- age_key %>%
  # Add age ranges
  full_join(reqs, by="age_group") %>%
  # Add
  mutate(nutrient_units=paste0(nutrient, " (", units, ")")) %>%
  mutate(sex_stage=paste0(sex, " (", stage, ")"),
         sex_stage=recode_factor(sex_stage,
                                 "Both (Infants)"="Infants",
                                 "Infants (Infants)"="Infants",
                                 "Both (Children)"="Children",
                                 "Children (Children)"="Children",
                                 "Females (Females)"="Women",
                                 "Females (None)"="Women",
                                 "Females (Pregnancy)"="Women (pregnant)",
                                 "Females (Lactation)"="Women (lactating)",
                                 "Males (Males)"="Men",
                                 "Males (None)"="Men")) %>%
  # Simplify
  select(nutrient_units, sex_stage, age_yr, endpoint, value) %>%
  # Arrange
  arrange(nutrient_units, sex_stage, age_yr, endpoint)



# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Plot data
g <- ggplot(reqs_expanded, aes(x=age_yr, y=value, color=sex_stage, group=sex_stage)) +
  facet_wrap(~nutrient_units, scales="free_y", ncol=4) +
  geom_path() +
  # Labels
  labs(x="Age (yr)", y="Average Requirement (AR)") +
  # Legend
  scale_color_discrete(name="Life stage") +
  # Limits
  scale_y_continuous(lim=c(0,NA)) +
  scale_x_continuous(breaks=c(0, 10, 24, 50, 70, 100)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS5_ar_values.png"),
       width=6.5, height=8, units="in", dpi=600)






