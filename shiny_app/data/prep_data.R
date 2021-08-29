
# Directories
datadir <- "data"
datadir_shiny <- "shiny_app/data"

# Read data
overlaps <- readRDS(file.path(datadir, "percent_overlap_among_country_pairs.Rds"))
dists <- readRDS(file.path(datadir, "nutrient_intake_distributions_23countries_expanded.Rds"))

# Export data
saveRDS(overlaps, file=file.path(datadir_shiny, "percent_overlap_among_country_pairs.Rds"))
saveRDS(dists, file=file.path(datadir_shiny, "nutrient_intake_distributions_23countries_expanded.Rds"))


# Read SPADE output and chop up by country
spade_output <- readRDS(file.path(datadir, "SPADE_output_merged_but_downscaled.Rds"))
countries <- sort(unique(spade_output$country))
for(i in 1:length(countries)){
  country_do <- countries[i]
  sdata <- spade_output %>%
    filter(country==country_do)
  iso3 <- sdata$iso3 %>% unique()
  filename <- paste0("SPADE_output_", iso3, ".Rds")
  saveRDS(sdata, file.path(datadir_shiny, filename))
}
