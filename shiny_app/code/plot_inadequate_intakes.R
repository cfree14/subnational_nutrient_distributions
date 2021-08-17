
# Plot inadequate intakes
# data <- dists_full; nutrient <- "Calcium"
plot_inadequate_intakes <- function(data, nutrient, base_theme){

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

  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=country, fill=sev)) +
    facet_wrap(~sex, drop = F) +
    geom_tile() +
    # Labels
    labs(x="Age group", y="") +
    # Legend
    scale_fill_gradientn("Prevalence of\ninadequate intakes",
                         lim=c(0,100),
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Scales
    scale_x_discrete(drop=F) +
    scale_y_discrete(drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="top",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g


}
