
# Plot coverage
# data <- dists_full; country <- "United States"
plot_intake_dists_cntry <- function(data, country, ul_yn, base_theme){

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
    # Ensure that all nutrients are plotted
    mutate(nutrient=factor(nutrient, levels=nutrients))

  # Generate distributions
  sdata_sim <- nutriR::generate_dists(sdata, perc=0.99)

  # Calculate EARs
  ears <- sdata %>%
    group_by(nutrient, sex) %>%
    summarize(ear=median(ear))

  # Calculate ULs
  uls <- sdata %>%
    group_by(nutrient, sex) %>%
    summarize(ul=median(ul_h))

  # Plot data
  g <- ggplot(sdata_sim, aes(x=intake, y=density, color=age_group, linetype=sex)) +
    facet_wrap(~nutrient, scales="free", drop = F, ncol=4) +
    geom_line() +
    # Plot EAR
    geom_vline(data=ears, mapping=aes(xintercept=ear, linetype=sex), show.legend=F) +
    # Labels
    labs(x="Habitual intake", y="Density", title=paste("Habitual intake distributions for:", country_do)) +
    # Legend
    scale_color_ordinal(name="Age group") +
    scale_linetype(name="Sex") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # Plot UL
    if(ul_yn=="Yes"){geom_vline(data=uls, mapping=aes(xintercept=ul, linetype=sex), color="red", show.legend=F)}
  g

}
