
# Directories
datadir <- "data"
datadir_shiny <- "shiny_app/data"

# Read overlaps
overlaps <- readRDS(file.path(datadir, "percent_overlap_among_country_pairs.Rds"))

# Export data
saveRDS(overlaps, file=file.path(datadir_shiny, "percent_overlap_among_country_pairs.Rds"))
