
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

  # Nutrient units
  nutrient_units <- sdata$nutrient_units %>% unique()

  # Ears
  ears <- sdata %>%
    select(sex, age_group,  ear) %>%
    unique()

  # Plot data
  y_label <- paste0("Mean intake (", nutrient_units, ")")
  g <- ggplot(sdata, aes(x=age_group, y=mu, fill=sev)) +
    facet_grid(country~sex, drop = F) +
    geom_bar(stat="identity", color="black", lwd=0.2) +
    # Plot EARs
    geom_line(data=ears, mapping=aes(x=age_group, y=ear, group=sex), inherit.aes=F) +
    # Labels
    labs(x="Age group", y=y_label) +
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