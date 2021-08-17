
# Plot coverage
# data <- dists_full; nutrient <- "Calcium"
plot_intake_dists <- function(data, nutrient, base_theme){

  # Nutrient
  nutrient_do <- nutrient

  # Parameters
  sexes <- data$sex %>% unique()
  age_groups <- data$age_group %>% unique()
  countries <- data$country %>% unique()

  # Subset data
  sdata <- data %>%
    # Reduce to nutrient of interest
    filter(nutrient==nutrient_do) %>%
    # Ensure that all countries are plotted
    mutate(country=factor(country, levels=countries))

  # Nutrient units
  nutrient_units <- sdata$nutrient_units %>% unique()

  # Generate distributions
  sdata_sim <- nutriR::generate_dists(sdata)

  # Plot data
  x_label <- paste0("Habitual intake (", nutrient_units, ")")
  g <- ggplot(sdata_sim, aes(x=intake, y=density, color=age_group, linetype=sex)) +
    facet_wrap(~country, scales="free", drop = F, ncol=4) +
    geom_line() +
    # Labels
    labs(x=x_label, y="Density") +
    # Legend
    scale_color_ordinal(name="Age group") +
    scale_linetype(name="Sex") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="top",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g

}
