
# Plot coverage
# data <- dists_full; nutrient <- "Iron"; country <- "United States"
plot_fits_over_obs <- function(data, country, nutrient, base_theme, datadir){

  # Nutrient / country
  country_do <- country
  nutrient_do <- nutrient

  # Parameters
  sexes <- data$sex %>% unique()
  age_groups <- data$age_group %>% unique()
  countries <- data$country %>% unique()

  # Fitted distributions
  ####################################

  # Subset fitted distributions
  sdata <- data %>%
    # Reduce to nutrient of interest
    filter(nutrient==nutrient_do & country==country_do)

  # Generate distributions
  sdata_sim <- nutriR::generate_dists(sdata, perc=0.99) %>%
    mutate(sex=factor(sex, levels=sexes))

  # SPADE output
  ####################################

  # Read country data
  iso3_do <- sdata$iso3 %>% unique()
  filename <- paste0("SPADE_output_", iso3_do, ".Rds")
  spade_output_orig <- readRDS(file.path(datadir, filename))

  # Format
  spade_output <- spade_output_orig %>%
    filter(nutrient==nutrient_do & sex %in% sexes & age_group %in% age_groups) %>%
    mutate(sex=factor(sex, levels=sexes),
           age_group=factor(age_group, levels=age_groups))

  # Plot
  ####################################

  # Nutrient units
  nutrient_units <- sdata$nutrient_units %>% unique()

  # Maximum value
  xmax <- sdata_sim$intake %>% max()

  # Plot data
  x_label <- paste0("Habitual intake (", nutrient_units, ")")
  title <- paste("SPADE output and distribution fits for:", nutrient_do, "intakes in the", country_do)
  g <- ggplot() +
    facet_wrap(~age_group, drop=F) +
    # Plot SPADE output
    geom_density(data=spade_output, mapping=aes(x=intake, fill=sex), alpha=0.5, color=NA) +
    # Plot distribution fits
    geom_line(data=sdata_sim, mapping=aes(x=intake, y=density, color=sex)) +
    # Limits
    lims(x=c(0, xmax)) +
    # Labels
    labs(x=x_label, y="Density", title=title) +
    # Legend
    scale_fill_discrete(name="SPADE output", drop=F) +
    scale_color_discrete(name="Distribution fit", drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "bottom")
  g

}
