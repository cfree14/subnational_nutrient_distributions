
# Plot coverage
# data <- dists_full; country <- "United States"
plot_coverage_by_country <- function(data, country, base_theme){

  # Country
  country_do <- country

  # Parameters
  sexes <- data$sex %>% unique()
  age_groups <- data$age_group %>% unique()
  nutrients <- data$nutrient %>% unique() %>% sort()

  # Subset data
  sdata <- data %>%
    # Reduce to country of interest
    filter(country==country_do) %>%
    # Summarize availability by sex
    group_by(nutrient_type, nutrient, age_group) %>%
    summarise(sexes=paste(sort(unique(sex)), collapse="-")) %>%
    ungroup() %>%
    # Recode sexes
    mutate(sexes=recode_factor(sexes,
                               "Males"="Men only",
                               "Females"="Women only",
                               "Females-Males"="Both men/women")) %>%
    # Ensure that all nutrients are plotted
    mutate(nutrient=factor(nutrient, levels=nutrients))

  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=nutrient, fill=sexes)) +
    # facet_grid(nutrient_type~., space="free_y", scales="free_y", drop=T) +
    geom_tile() +
    # Labels
    labs(x="Age group", y="", title=paste("Data coverage for:", country_do)) +
    scale_fill_discrete(name="", drop=F) +
    # Scales
    scale_x_discrete(drop=F) +
    scale_y_discrete(drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g


}
