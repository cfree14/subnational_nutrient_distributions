
# Plot coverage
# data <- dists_full; nutrient <- "Iron"; scales <- "Fixed"
plot_intake_dists <- function(data, nutrient, scales="Fixed", base_theme){

  # Nutrient
  nutrient_do <- nutrient

  # Parameters
  sexes <- data$sex %>% unique()
  age_groups <- data$age_group %>% unique()
  countries <- data$country %>% unique()

  # X-axis scales
  if(scales=="Fixed"){
    scales_use <- "free_y"
  }else{
    scales_use <- "free"
  }

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

  # Calculate EARs
  ears <- sdata %>%
    group_by(sex) %>%
    summarize(ear=median(ear))

  # Plot data
  x_label <- paste0("Habitual intake (", nutrient_units, ")")
  g <- ggplot(sdata_sim, aes(x=intake, y=density, color=age_group, linetype=sex)) +
    facet_wrap(~country, scales=scales_use, drop = F, ncol=4) +
    geom_line() +
    # Plot EAR
    geom_vline(data=ears, mapping=aes(xintercept=ear, linetype=sex), show.legend=F) +
    # Labels
    labs(x=x_label, y="Density", title=paste("Habitual intake distributions for:", nutrient_do)) +
    # Legend
    scale_color_ordinal(name="Age group") +
    scale_linetype(name="Sex") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g

}
