
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
    filter(nutrient==nutrient_do)

  # Generate distributions
  sdata_sim <- nutriR::generate_dists(sdata)

  # Plot data
  g <- ggplot(sdata_sim, aes(x=intake, y=density, color=age_group)) +
    facet_grid(country~sex) +
    geom_line() +
    # Labels
    labs(x="Habitual intake", y="Density") +
    # Legend
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g

}
