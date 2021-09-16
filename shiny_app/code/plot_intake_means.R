
# Plot coverage
# data <- dists_full; nutrient <- "Calcium"
plot_intake_means <- function(data, nutrient, scales, ul_yn, base_theme){

  # Nutrient
  nutrient_do <- nutrient

  # Parameters
  sexes <- data$sex %>% unique()
  age_groups <- data$age_group %>% unique()
  countries <- data$country %>% unique() %>% sort()

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

  # EARs
  ears <- sdata %>%
    select(sex, age_group,  ear) %>%
    unique()

  # ULs
  uls <- sdata %>%
    select(sex, age_group, ul_h) %>%
    unique() %>%
    rename(ul=ul_h)

  # Plot data
  y_label <- paste0("Mean intake (", nutrient_units, ")")
  g <- ggplot(sdata, aes(x=age_group, y=mu, fill=sev)) +
    facet_grid(country~sex, drop = F, scales=scales_use) +
    geom_bar(stat="identity", color="black", lwd=0.2) +
    # Plot EARs
    geom_line(data=ears, mapping=aes(x=age_group, y=ear, group=sex), inherit.aes=F) +
    # Labels
    labs(x="Age group", y=y_label, title=paste("Mean habitual intakes and EARs for:", nutrient_do)) +
    # Scales
    lims(y=c(0, NA)) +
    scale_x_discrete(drop=F) +
    # Legend
    scale_fill_gradientn("Prevalence of\ninadequate intakes",
                         lim=c(0,100),
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # Plot ULs
    if(ul_yn=="Yes"){geom_line(data=uls, mapping=aes(x=age_group, y=ul, group=sex), color="red", inherit.aes=F)}
  g


}
