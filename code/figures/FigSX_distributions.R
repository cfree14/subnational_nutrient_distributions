

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures/dists"
tabledir <- "tables"

# Read distribution key
dists <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries.Rds")) %>%
  filter(best_dist!="none")


# Simulate distributions
################################################################################

# Loop through distributions and build data
data_orig <- purrr::map_df(1:nrow(dists), function(i){

  # Distribution to do
  dist_do <- dists$best_dist[i]
  country_do <- dists$country[i]
  nutrient_do <- dists$nutrient[i]
  sex_do <- dists$sex[i]
  age_group_do <- dists$age_group[i]

  # Simulation range
  xmin <- 0

  # If gamma
  if(dist_do=="gamma"){

    # Extract parameters
    shape <- dists$g_shape[i]
    rate <- dists$g_rate[i]

    # Build curve
    xmax <- qgamma(0.99, shape=shape, rate=rate)
    x <- seq(xmin, xmax, length.out = 200)
    y <- dgamma(x, shape=shape, rate=rate)
    # plot(y ~ x)

    # Build data frame
    df <- tibble(country=country_do,
                 nutrient=nutrient_do,
                 sex=sex_do,
                 age_group=age_group_do,
                 intake=x,
                 density=y)

  }

  # If log-normal
  if(dist_do=="log-normal"){

    # Extract parameters
    meanlog <- dists$ln_meanlog[i]
    sdlog <- dists$ln_sdlog[i]

    # Build curve
    xmax <- qlnorm(0.99, meanlog=meanlog, sdlog=sdlog)
    x <- seq(xmin, xmax, length.out = 200)
    y <- dlnorm(x, meanlog=meanlog, sdlog=sdlog)
    # plot(y ~ x)

    # Build data frame
    df <- tibble(country=country_do,
                 nutrient=nutrient_do,
                 sex=sex_do,
                 age_group=age_group_do,
                 intake=x,
                 density=y)

  }

  # Return
  df

})

# Format data
data <- data_orig %>%
  filter(country!='Bulgaria') %>%
  mutate(age_group_lo = gsub("\\-.*", "", age_group) %>% as.numeric(),
         group=paste(sex, age_group_lo, sep="-"),
         sex=factor(sex, levels=c("Males", "Females")))



# Build figures
################################################################################

# Nutrients
nutrients <- sort(unique(data$nutrient))

# Loop through nutrients
for(i in 1:length(nutrients)){

  # Subset data
  nutrient_do <- nutrients[i]
  nutrient_units <- dists %>%
    filter(nutrient==nutrient_do) %>%
    pull(nutrient_units) %>% unique()
  sdata <- data %>%
    filter(nutrient==nutrient_do)

  # Plot data
  xlabel <- paste0("Habitual intake (", nutrient_units, "/d)")
  g <- ggplot(sdata, aes(x=intake, y=density, color=age_group_lo, linetype=sex, group=group)) +
    facet_wrap(~country, scales="free_y", ncol=5) +
    geom_line() +
    # Labels
    labs(x=xlabel, y="Density", title=nutrient_do) +
    # Scale
    scale_linetype_discrete(name="Sex") +
    scale_color_gradientn(name="Age group (yr)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
    guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() +
    theme(axis.text.y = element_blank(),
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
          legend.position="bottom")
  g

  # Number of countries and dimension
  ncntry <- n_distinct(sdata$country)
  nrows <- ceiling(ncntry/5)
  height <- c(1+nrows*1.5)

  # Export
  outfile <- paste0("FigSX_intake_dist_", tolower(nutrient_do) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir, outfile),
         width=6.5, height=height, units="in", dpi=600)


}







