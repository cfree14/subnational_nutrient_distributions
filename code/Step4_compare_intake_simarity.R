

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "nutrient_intake_distributions_22countries_expanded.Rds"))


# Similarity among countries
################################################################################

# Compare sex-age in country A to country B-X
key1 <- data_orig %>%
  filter(sex!="Children") %>%
  group_by(nutrient_type, nutrient, sex, age_group) %>%
  summarize(ncountries=n()) %>%
  ungroup() %>%
  filter(ncountries>1)

# Compute mean percent overlaps
# For testing: x <- 1
data1 <- purrr::map_df(1:nrow(key1), function(x){

  # Parameters
  nutrient_do <- key1$nutrient[x]
  sex_do <- key1$sex[x]
  age_do <- key1$age_group[x]
  nutrient_type <- key1$nutrient_type[x]

  # Get distributions
  dists <- data_orig %>%
    # Reduce to nutrient of interest
    filter(nutrient==nutrient_do & sex==sex_do & age_group==age_do)

  # Build pairwise comparison key
  compare_key <- combn(dists$iso3, 2) %>%
    # Transpose
    t() %>%
    # Convert to dataframe
    as.data.frame() %>%
    # Rename
    rename(iso1=V1, iso2=V2)

  # Loop through each pairwise combination and calculate percent overlap
  # For testing: y <- 1
  poverlaps_group <- purrr::map_df(1:nrow(compare_key), function(y){

    # Build distribution 1
    iso1 <- compare_key$iso1[y]
    dist1_type <- dists %>%
      filter(iso3==iso1) %>%
      pull(best_dist)
    if(dist1_type=="gamma"){
      shape <-  dists %>%
        filter(iso3==iso1) %>%
        pull(g_shape)
      rate <- dists %>%
        filter(iso3==iso1) %>%
        pull(g_rate)
      dist1 <- list(shape=shape, rate=rate)
    }else{
      meanlog <-  dists %>%
        filter(iso3==iso1) %>%
        pull(ln_meanlog)
      sdlog <- dists %>%
        filter(iso3==iso1) %>%
        pull(ln_sdlog)
      dist1 <- list(meanlog=meanlog, sdlog=sdlog)
    }

    # Build distribution 2
    iso2 <- compare_key$iso2[y]
    dist2_type <- dists %>%
      filter(iso3==iso2) %>%
      pull(best_dist)
    if(dist2_type=="gamma"){
      shape <-  dists %>%
        filter(iso3==iso2) %>%
        pull(g_shape)
      rate <- dists %>%
        filter(iso3==iso2) %>%
        pull(g_rate)
      dist2 <- list(shape=shape, rate=rate)
    }else{
      meanlog <-  dists %>%
        filter(iso3==iso2) %>%
        pull(ln_meanlog)
      sdlog <- dists %>%
        filter(iso3==iso2) %>%
        pull(ln_sdlog)
      dist2 <- list(meanlog=meanlog, sdlog=sdlog)
    }

    # Calculate percent overlap
    poverlap <- nutriR::overlap(dist1, dist2, plot=F)

    # Build data frame
    df <- data.frame(nutrient_type=nutrient_type,
                     nutrient=nutrient_do,
                     sex=sex_do, age=age_do,
                     iso1=iso1, iso2=iso2,
                     poverlap=poverlap)

  })

})

# Export data
saveRDS(data1, file=file.path(datadir, "percent_overlap_among_country_pairs.Rds"))

# Compute stats
stats1 <- data1 %>%
  # Calculate median percent overlap
  group_by(nutrient, sex, age) %>%
  summarise(poverlap=median(poverlap, na.rm=T)) %>%
  # Add nutrient type
  left_join(key, by=c("nutrient", "sex", "age"="age_group"))

# Plot
g <- ggplot(stats1, aes(y=nutrient, x=age, fill=poverlap)) +
  facet_grid(nutrient_type~sex, space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Median\npercent overlap", colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# Similarity among ages
################################################################################

# Compare country-sex in ages A to ages B-X
key2 <- data_orig %>%
  filter(sex!="Children") %>%
  group_by(nutrient_type, nutrient, country, iso3, sex) %>%
  summarize(nages=n()) %>%
  ungroup() %>%
  filter(nages>1)

# Compute mean percent overlaps
# For testing: x <- 1
data2 <- purrr::map_df(1:nrow(key2), function(x){

  # Parameters
  nutrient_do <- key2$nutrient[x]
  country_do <- key2$country[x]
  iso3_do <- key2$iso3[x]
  sex_do <- key2$sex[x]
  nutrient_type <- key2$nutrient_type[x]

  # Get distributions
  dists <- data_orig %>%
    # Reduce to nutrient of interest
    filter(nutrient==nutrient_do & iso3==iso3_do & sex==sex_do)

  # Build pairwise comparison key
  compare_key <- combn(dists$age_group %>% as.character(.), 2) %>%
    # Transpose
    t() %>%
    # Convert to dataframe
    as.data.frame() %>%
    # Rename
    rename(age_group1=V1, age_group2=V2)

  # Loop through each pairwise combination and calculate percent overlap
  # For testing: y <- 1
  poverlaps_group <- purrr::map_df(1:nrow(compare_key), function(y){

    # Build distribution 1
    age_group1 <- compare_key$age_group1[y]
    dist1_type <- dists %>%
      filter(age_group==age_group1) %>%
      pull(best_dist)
    if(dist1_type=="gamma"){
      shape <-  dists %>%
        filter(age_group==age_group1) %>%
        pull(g_shape)
      rate <- dists %>%
        filter(age_group==age_group1) %>%
        pull(g_rate)
      dist1 <- list(shape=shape, rate=rate)
    }else{
      meanlog <-  dists %>%
        filter(age_group==age_group1) %>%
        pull(ln_meanlog)
      sdlog <- dists %>%
        filter(age_group==age_group1) %>%
        pull(ln_sdlog)
      dist1 <- list(meanlog=meanlog, sdlog=sdlog)
    }

    # Build distribution 2
    age_group2 <- compare_key$age_group2[y]
    dist2_type <- dists %>%
      filter(age_group==age_group2) %>%
      pull(best_dist)
    if(dist2_type=="gamma"){
      shape <-  dists %>%
        filter(age_group==age_group2) %>%
        pull(g_shape)
      rate <- dists %>%
        filter(age_group==age_group2) %>%
        pull(g_rate)
      dist2 <- list(shape=shape, rate=rate)
    }else{
      meanlog <-  dists %>%
        filter(age_group==age_group2) %>%
        pull(ln_meanlog)
      sdlog <- dists %>%
        filter(age_group==age_group2) %>%
        pull(ln_sdlog)
      dist2 <- list(meanlog=meanlog, sdlog=sdlog)
    }

    # Calculate percent overlap
    poverlap <- nutriR::overlap(dist1, dist2, plot=F)

    # Build data frame
    df <- data.frame(nutrient_type=nutrient_type,
                     nutrient=nutrient_do,
                     country=country_do,
                     iso3=iso3_do,
                     sex=sex_do,
                     age_group1=age_group1,
                     age_group2=age_group2,
                     poverlap=poverlap)

  })

})

# Export data
saveRDS(data2, file=file.path(datadir, "percent_overlap_among_age_pairs.Rds"))

# Compute stats
stats2 <- data2 %>%
  # Calculate median percent overlap
  group_by(nutrient_type, nutrient, country, iso3, sex) %>%
  summarise(poverlap=median(poverlap, na.rm=T))

# Plot
g <- ggplot(stats2, aes(y=nutrient, x=country, fill=poverlap)) +
  facet_grid(nutrient_type~sex, space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Median\npercent overlap", colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g



# Similarity among sexes
################################################################################

# Compare country-sex in ages A to ages B-X
key3 <- data_orig %>%
  filter(sex!="Children") %>%
  group_by(nutrient_type, nutrient, country, iso3, age_group) %>%
  summarize(nsexes=n()) %>%
  ungroup() %>%
  filter(nsexes==2)

# Compute mean percent overlaps
# For testing: x <- 1
data3 <- purrr::map_df(1:nrow(key3), function(x){

  # Parameters
  nutrient_do <- key3$nutrient[x]
  country_do <- key3$country[x]
  iso3_do <- key3$iso3[x]
  age_group_do <- key3$age_group[x]
  nutrient_type <- key3$nutrient_type[x]

  # Get distributions
  dists <- data_orig %>%
    # Reduce to nutrient of interest
    filter(nutrient==nutrient_do & iso3==iso3_do & age_group==age_group_do)

  # Build pairwise comparison key
  compare_key <- combn(dists$sex %>% as.character(.), 2) %>%
    # Transpose
    t() %>%
    # Convert to dataframe
    as.data.frame() %>%
    # Rename
    rename(sex1=V1, sex2=V2)

  # Loop through each pairwise combination and calculate percent overlap
  # For testing: y <- 1
  poverlaps_group <- purrr::map_df(1:nrow(compare_key), function(y){

    # Build distribution 1
    sex1 <- compare_key$sex1[y]
    dist1_type <- dists %>%
      filter(sex==sex1) %>%
      pull(best_dist)
    if(dist1_type=="gamma"){
      shape <-  dists %>%
        filter(sex==sex1) %>%
        pull(g_shape)
      rate <- dists %>%
        filter(sex==sex1) %>%
        pull(g_rate)
      dist1 <- list(shape=shape, rate=rate)
    }else{
      meanlog <-  dists %>%
        filter(sex==sex1) %>%
        pull(ln_meanlog)
      sdlog <- dists %>%
        filter(sex==sex1) %>%
        pull(ln_sdlog)
      dist1 <- list(meanlog=meanlog, sdlog=sdlog)
    }

    # Build distribution 2
    sex2 <- compare_key$sex2[y]
    dist2_type <- dists %>%
      filter(sex==sex2) %>%
      pull(best_dist)
    if(dist2_type=="gamma"){
      shape <-  dists %>%
        filter(sex==sex2) %>%
        pull(g_shape)
      rate <- dists %>%
        filter(sex==sex2) %>%
        pull(g_rate)
      dist2 <- list(shape=shape, rate=rate)
    }else{
      meanlog <-  dists %>%
        filter(sex==sex2) %>%
        pull(ln_meanlog)
      sdlog <- dists %>%
        filter(sex==sex2) %>%
        pull(ln_sdlog)
      dist2 <- list(meanlog=meanlog, sdlog=sdlog)
    }

    # Calculate percent overlap
    poverlap <- nutriR::overlap(dist1, dist2, plot=F)

    # Build data frame
    df <- data.frame(nutrient_type=nutrient_type,
                     nutrient=nutrient_do,
                     country=country_do,
                     iso3=iso3_do,
                     age_group=age_group_do,
                     sex1=sex1,
                     sex2=sex2,
                     poverlap=poverlap)

  })

})

# Export data
saveRDS(data3, file=file.path(datadir, "percent_overlap_among_sex_pairs.Rds"))

# Compute stats
stats3 <- data3 %>%
  # Calculate median percent overlap
  group_by(nutrient_type, nutrient, age_group) %>%
  summarise(poverlap=median(poverlap, na.rm=T))

# Plot
g <- ggplot(stats3, aes(y=nutrient, x=age_group, fill=poverlap)) +
  facet_grid(nutrient_type~., space="free_y", scales="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="Median\npercent overlap", colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd"))) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# # Plot
# g <- ggplot(key3, aes(y=nutrient, x=age_group, fill=nsexes)) +
#   facet_grid(nutrient_type~., space="free_y", scales="free_y") +
#   geom_raster() +
#   # Labels
#   labs(x="", y="") +
#   # Theme
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# g

