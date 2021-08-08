
# Plot coverage
# data <- dists_full; nutrient <- "Calcium"
plot_intake_means <- function(data, nutrient, base_theme){

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
  g <- ggplot(sdata, aes(x=age_group, y=mu, fill=sev)) +
    facet_grid(country~sex, scales="free_y") +
    geom_bar(stat="identity", color="grey30", stroke=0.5) +
    # Labels
    labs(x="Age group", y="Mean intake") +
    # Scales
    scale_x_discrete(drop=F) +
    # Legend
    scale_fill_gradientn("Prevalence of\ninadequate intakes",
                         lim=c(0,100),
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="top",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g


}
