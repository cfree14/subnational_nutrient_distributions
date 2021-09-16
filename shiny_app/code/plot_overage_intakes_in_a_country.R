
# Plot inadequate intakes
# data <- dists_full; country <- "United States"
plot_overage_intakes_in_a_country <- function(data, country, base_theme){

  # County
  country_do <- country

  # Parameters
  sexes <- data$sex %>% unique()
  age_groups <- data$age_group %>% unique()
  nutrients_with_uls <- data$nutrient[!is.na(data$ul_h)] %>% unique() %>% sort()

  # Subset data
  sdata <- data %>%
    # Reduce to country of interest
    filter(country==country_do & nutrient %in% nutrients_with_uls) %>%
    # Ensure that all nutrients are plotted
    mutate(nutrient=factor(nutrient, levels=nutrients_with_uls))

  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=nutrient, fill=above_ul)) +
    facet_wrap(~sex, drop = F) +
    geom_tile() +
    # Labels
    labs(x="Age group", y="", title=paste("Prevalence of intakes over upper limits in:", country_do)) +
    # Legend
    scale_fill_gradientn("Prevalence of intakes\nover upper limit",
                         lim=c(0,100),
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd"), na.value="white") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Scales
    scale_x_discrete(drop=F) +
    scale_y_discrete(drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g


}
