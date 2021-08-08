
# Plot coverage
# data <- dists_full; nutrient <- "Calcium"
plot_coverage <- function(data, nutrient, base_theme){

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
    # Summarize availability by sex
    group_by(country, age_group) %>%
    summarise(sexes=paste(sort(unique(sex)), collapse="-")) %>%
    ungroup() %>%
    # Recode sexes
    mutate(sexes=recode_factor(sexes,
                               "Males"="Men only",
                               "Females"="Women only",
                               "Females-Males"="Both men/women")) %>%
    # Ensure that all countries are plotted
    mutate(country=factor(country, levels=countries))

  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=country, fill=sexes)) +
    geom_tile() +
    # Labels
    labs(x="Age group", y="") +
    scale_fill_discrete(name="") +
    # Scales
    scale_x_discrete(drop=F) +
    scale_y_discrete(drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="top",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g


}
