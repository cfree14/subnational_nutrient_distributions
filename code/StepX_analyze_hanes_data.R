

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(ggbiplot)


# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/old_nhanes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures/nhanes"

# Read data
data_orig <- readRDS(file=file.path(datadir, "nhanes_spade_output.Rds"))

# Read nutrient key
nutr_key <- readxl::read_excel(file.path(datadir, "SPADE_nutrient_key.xlsx"))


# Plot data
################################################################################

# Define age groups - shouldn't need to
age_group_lo <- c(0, seq(5,100,5))
age_group_hi <- c(seq(4,99,5), Inf)
age_groups <- paste(age_group_lo, age_group_hi, sep="-")
age_group_breaks <- c(-Inf,age_group_hi)

# Sample data
data_plot <- sample_frac(data_orig, size=0.1) %>%
  mutate(age_group=cut(age_yr, breaks=age_group_breaks, labels=age_groups),
         sex=factor(sex, levels=c("Males", "Females")),
         group=paste(sex, year, sep="-"))

# Nutrients
nutrients <- sort(unique(data_plot$nutrient))

# Loop through nutrients
for(i in 1:length(nutrients)){

  # Subset data
  nutrient_do <- nutrients[i]
  sdata <- data_plot %>%
    filter(nutrient==nutrient_do)

  # Theme
  my_theme <-my_theme <-  theme(axis.text.y = element_blank(),
                                axis.text=element_text(size=6),
                                axis.title=element_text(size=8),
                                legend.text=element_text(size=6),
                                legend.title=element_text(size=8),
                                strip.text=element_text(size=8),
                                plot.title=element_text(size=10),
                                # Gridlines
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                axis.line = element_line(colour = "black"),
                                # Legend
                                legend.position="bottom")

  # Plot data
  # g <- ggplot(sdata, aes(x=intake, color=year, group=year)) +
  #   facet_grid(age_group~sex) +
  #   geom_density() +
  #   # Labels
  #   labs(x="Habitual intake", y="Density\n(values not shown)", title=nutrient_do) +
  #   scale_color_gradientn(name="Year", breaks=seq(2008,2016, 2), colors=RColorBrewer::brewer.pal(n=9, name='Greens')[2:9]) +
  #   guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  #   # Theme
  #   theme_bw() +  my_theme
  # g

  # # Plot data (Option #2)
  # g <- ggplot(sdata, aes(x=intake, color=sex, group=group)) +
  #   facet_wrap(~age_group, ncol=4) +
  #   geom_density() +
  #   # Labels
  #   labs(x="Habitual intake", y="Density\n(values not shown)", title=nutrient_do) +
  #   scale_color_discrete(name="Sex") +
  #   scale_alpha_continuous(name="Year", breaks=seq(2008,2016,2), range=c(0.1, 1)) +
  #   # Theme
  #   theme_bw() +  my_theme
  # g

  # Plot data
  g <- ggplot(sdata, aes(x=intake, color=year, linetype=sex, group=group)) +
    facet_wrap(~age_group, ncol=4) +
    geom_density() +
    # Labels
    labs(x="Habitual intake", y="Density\n(values not shown)", title=nutrient_do) +
    # Legends
    scale_linetype(name="Sex") +
    scale_color_gradientn(name="Year", breaks=seq(2008,2016, 2), colors=RColorBrewer::brewer.pal(n=9, name='Greens')[2:9]) +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() +  my_theme
  g

  # Number of age groups and figure dimensions
  nages <- n_distinct(sdata$age_group)
  nrows <- ceiling(nages / 4)
  height <- c(1+nrows*1.5)

  # Export
  outfile <- paste0("FigSX_intake_dist_", tolower(nutrient_do) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir, outfile),
         width=6.5, height=height, units="in", dpi=600)



}

