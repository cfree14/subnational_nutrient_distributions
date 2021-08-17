
# Plot coverage
# data <- dists_full; nutrient <- "Calcium"
plot_intake_dists_age_group <- function(data, nutrient, base_theme){

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

  # Build overlap labels
  overlap_pos <- sdata_sim %>%
    # Calculate y for label
    group_by(nutrient, sex, age_group) %>%
    summarize(intake_max=max(intake),
              density_max=max(density))

  # Build overlap data
  overlaps_plot <- overlaps %>%
    # Nutrient of interest
    filter(nutrient==nutrient_do) %>%
    # Add position
    left_join(overlap_pos) %>%
    # Format percent overlap
    mutate(overlap_label=round(overlap, digits=0) %>% paste0(., "%"))

  # Plot data
  x_label <- paste0("Habitual intake (", nutrient_units, ")")
  g1 <- ggplot(sdata_sim %>% filter(sex=="Males"), aes(x=intake, y=density, color=country)) +
    facet_wrap(~age_group, scales="free", ncol=4, drop = F) +
    geom_line() +
    # Plot overlap text
    geom_text(data=overlaps_plot %>% filter(sex=="Males"),
              mapping=aes(x=intake_max, y=density_max, label=overlap_label),
              inherit.aes=F, size=4, hjust=1.1, vjust=1.1) +
    # Labels
    labs(x=x_label, y="Density", title="Males") +
    # Legend
    scale_color_discrete(name="Country", drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g1

  # Plot data
  g2 <- ggplot(sdata_sim %>% filter(sex=="Females"), aes(x=intake, y=density, color=country)) +
    facet_wrap(~age_group, scales="free", ncol=4, drop = F) +
    geom_line() +
    # Plot overlap text
    geom_text(data=overlaps_plot %>% filter(sex=="Females"),
              mapping=aes(x=intake_max, y=density_max, label=overlap_label),
              inherit.aes=F, size=4, hjust=1.1, vjust=1.1) +
    # Labels
    labs(x=x_label, y="Density", title="Females") +
    # Legend
    scale_color_discrete(name="Country", drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g2

  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=1)


}
