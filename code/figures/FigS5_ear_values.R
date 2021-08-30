

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

# DRIs
dris <- nutriR::dris



# Build data
################################################################################

# Build data
ears <- dris %>%
  # Reduce to EARs
  filter(dri_type=="Estimated Average Requirement (EAR)")

# Build age key
age_key <- ears %>%
  # Unique ages
  select(age_range) %>%
  unique() %>%
  # Recode
  mutate(age_range_simple=recode(age_range,
                                 "0-6 mo"="0-0.5 yr",
                                 "6-12 mo"="0.5-1 yr",
                                 ">70 yr"="71-100 yr") %>% gsub(" yr", "", .)) %>%
  # Seperate
  tidyr::separate(age_range_simple, sep="-", into=c("age1", "age2"), remove=F) %>%
  # Gather
  select(age_range, age_range_simple, age1, age2) %>%
  gather(key="endpoint", value='age_yr', 3:4) %>%
  mutate(endpoint=ifelse(endpoint=="age1", 1, 2),
         age_yr=age_yr %>% as.numeric())

# Expand EARs
ears_expanded <- age_key %>%
  # Add age ranges
  full_join(ears, by="age_range") %>%
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
                   legend.position="right")

# Plot data
g <- ggplot(ears_expanded, aes(x=age_yr, y=value, color=sex_stage, group=sex_stage)) +
  facet_wrap(~nutrient_units, scales="free_y", ncol=3) +
  geom_path() +
  # Labels
  labs(x="Age (yr)", y="Estimated Average Requirement (EAR)") +
  # Legend
  scale_color_discrete(name="Life stage") +
  # Limits
  scale_y_continuous(lim=c(0,NA)) +
  scale_x_continuous(breaks=c(0, 8, 18, 30, 50, 70, 100)) +
  # Theme
  theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigS5_ear_values.png"),
       width=6.5, height=7, units="in", dpi=600)






