

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(ggbiplot)
library(vegan)
library(factoextra)


# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read file key
file_key <- read.csv(file=file.path(datadir, "SPADE_file_key.csv"), as.is=T) %>%
  filter(country!="Bulgaria")

# Read GENUS data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/nutrient_endowment/data/genus/processed/genus_edible_food_by_cntry_year.Rds")

# Read map
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")


# Build data for multivariate analysis
################################################################################

# Build data
data <- data_orig %>%
  # Filter (remove NAs and columns with zeros)
  filter(year==2011 & !is.na(g_person_day) & g_person_day>0) %>%
  # Reduce
  select(iso3_use, country_use, food, g_person_day) %>%
  # Rename
  rename(iso3=iso3_use, country=country_use, food=food, supply_g=g_person_day) %>%
  spread(key="food", value="supply_g") %>%
  # Fill missing values
  replace(is.na(.), 0) %>%
  # Add
  mutate(continent=countrycode(iso3, "iso3c", "continent")) %>%
  select(continent, country, iso3, everything())

# Convert to matrix
data_mat <- data %>%
  column_to_rownames(var="iso3") %>%
  select(-c(continent, country)) %>%
  as.matrix()

# Country key
cntry_key <- data %>%
  select(continent, country, iso3) %>%
  unique() %>%
  arrange(continent, iso3)

# Identify continent seperators
cntry_key_seps <- cntry_key %>%
  group_by(continent) %>%
  slice(1)


# Compute and visualize dissimilarity matrix
################################################################################

# Compute dissimilarity
# diss_vec <- get_dist(data_mat, method = "euclidean")
diss_vec <- vegan::vegdist(data_mat, method = "euclidean", upper=T)
diss_mat <- as.matrix(diss_vec)
diff_df <- diss_mat %>%
  as.data.frame() %>%
  rownames_to_column(var="iso1") %>%
  select(iso1, everything()) %>%
  gather(key="iso2", value="diss", 2:ncol(.)) %>%
  mutate(iso1=factor(iso1, levels=cntry_key$iso3),
         iso2=factor(iso2, levels=cntry_key$iso3))

# Plot dissimilarity
g <- ggplot(diff_df, aes(x=iso1, y=iso2, fill=diss)) +
  geom_raster(alpha=0.7) +
  # Lines
  geom_hline(yintercept = cntry_key_seps$iso3) +
  geom_vline(xintercept = cntry_key_seps$iso3) +
  geom_text(data=cntry_key_seps, mapping=aes(x=iso3, y=iso3, label=continent), inherit.aes = F,
            hjust=-0.1, vjust=-0.5) +
  # Labels
  labs(x="", y="") +
  scale_fill_gradientn(name="Euclidean\ndistance", colors=RColorBrewer::brewer.pal(n=9, name="YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=3),
        axis.title=element_text(size=6),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigSX_genus_diss_matrix.png"),
       width=6.5, height=6.5, units="in", dpi=600)


# Search for best match
################################################################################

# Build intake group key
intake_key <- diff_df %>%
  # Mark countries with data
  mutate(reference=ifelse(iso2 %in% file_key$iso3, "yes", "no")) %>%
  filter(reference=="yes") %>%
  # Identify best match
  group_by(iso1) %>%
  arrange(iso1, diss) %>%
  slice(1) %>%
  ungroup() %>%
  # Format
  rename(iso3=iso1, group_iso3=iso2, dissimilarity=diss) %>%
  mutate(country=countrycode(iso3, "iso3c", "country.name"),
         group_country=countrycode(group_iso3, "iso3c", "country.name")) %>%
  # Arrange
  select(group_country, group_iso3, country, iso3, dissimilarity) %>%
  arrange(group_country, dissimilarity)


# Plot distribution groups
################################################################################

# Map assignments
world1 <- world %>%
  select(gu_a3) %>%
  rename(iso3=gu_a3) %>%
  left_join(intake_key, by=c("iso3"))

# Countries with data
centroids <- world %>%
  select(gu_a3) %>%
  rename(iso3=gu_a3) %>%
  filter(iso3 %in% file_key$iso3) %>%
  mutate(country=countrycode(iso3, "iso3c", "country.name")) %>%
  sf::st_centroid() %>%
  mutate(long_dd=sf::st_coordinates(.)[,1],
         lat_dd=sf::st_coordinates(.)[,2])

# Plot
g1 <- ggplot() +
  geom_sf(world1, mapping=aes(fill=group_country), color="grey30", lwd=0.2) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Plot/label centroids
  # geom_sf(data=centroids, size=1.2) +
  # ggrepel::geom_text_repel(data=centroids, mapping=aes(x=long_dd, y=lat_dd, label=country),
  #                          min.segment.length = 0, size=2, segment.size=0.2) +
  # Legend
  labs(title="Group assignment based on most similar country with data") +
  scale_fill_discrete(name="Group", na.value="grey80") +
  # Theme
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        plot.title = element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.25, "cm"))
g1

# Export figure
ggsave(g1, filename=file.path(plotdir, "FigSX_dist_groups_map_possible.png"),
       width=6.5, height=2.5, units="in", dpi=600)


# Idealized groups identified using K-means
################################################################################

# K-means using factoextra
# https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/

# CLustering tutorial
# https://compgenomr.github.io/book/clustering-grouping-samples-based-on-their-similarity.html#k-means-clustering

# Fit k-means using maximum possible groups given data (21 countries)
kmeans_fit <- eclust(data_mat, "kmeans", k.max=21)

# Inspect gap statistic
kmeans_fit$nbclust
fviz_gap_stat(kmeans_fit$gap_stat)

# Inspect silhouette
fviz_silhouette(kmeans_fit)

# Extract cluster assignments
cluster_vec <- kmeans_fit$cluster

# Build cluster dataframe
cluster_df <- kmeans_fit$clust_plot$data %>%
  rename(iso3=name) %>%
  mutate(cluster_long=paste("Cluster", cluster)) %>%
  select(iso3, cluster_long, cluster, everything())


# Plot idealized groups identified using K-means
################################################################################

# Add to world and plot
world2 <- world %>%
  left_join(cluster_df, by=c("gu_a3"="iso3"))

# Plot
g2 <- ggplot() +
  geom_sf(world2, mapping=aes(fill=cluster_long), color="grey30", lwd=0.2) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Plot/label centroids
  # geom_sf(data=centroids, size=1.2) +
  # ggrepel::geom_text_repel(data=centroids, mapping=aes(x=long_dd, y=lat_dd, label=country),
  #                          min.segment.length = 0, size=2, segment.size=0.2) +
  # Legend
  labs(title="Idealized group assignments based on K-means clustering (but limited by available data)") +
  scale_fill_discrete(name="Group", na.value="grey80") +
  # Theme
  theme_bw() +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        plot.title = element_text(size=6),
        # Gridlines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.25, "cm"))
g2

# Export figure
ggsave(g2, filename=file.path(plotdir, "FigSX_dist_groups_map_idealized.png"),
       width=6.5, height=2.5, units="in", dpi=600)


# Merged plot
################################################################################

# Merge plot
g <- gridExtra::grid.arrange(g1, g2, nrow=2)

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_dist_groups_map_merged.png"),
       width=6.5, height=5, units="in", dpi=600)



# # Hierarchical clustering
# ################################################################################
#
# # Compute hierarchical clustering
# hc_fit <- eclust(data_mat, "hclust", k.max=7)
#
# # Inspect dendrogram
# fviz_dend(hc_fit, rect = TRUE) # dendrogam
#
# # Inspect gap
# fviz_gap_stat(hc_fit$gap_stat)
#
# # Inspect grouping
# fviz_cluster(hc_fit)
#
#
# # Extract cluster assignments
# # str(kmeans_fit)
# cluster_vec <- hc_fit$cluster
# cluster_df <- tibble(iso3=names(cluster_vec),
#                      cluster=cluster_vec %>% as.character())
#
# # Add to world and plot
# world2 <- world %>%
#   left_join(cluster_df, by=c("gu_a3"="iso3"))
#
# # Plot
# g <- ggplot() +
#   geom_sf(world2, mapping=aes(fill=cluster)) +
#   theme_bw()
# g
#
#
#
#
#
# # PCA play
# ################################################################################
#
#
#
#
# # Fit PCA
# pca_fit <- prcomp(data_mat)
#
# # Plot PCA data
# ggbiplot(pca_fit, labels=rownames(data_mat), var.axes=F) +
#   theme_bw()
#
# # Extract PCA info for plotting
# data_pca <- tibble(iso3=rownames(data_mat),
#                    pc1=scale(pca_fit$x[,"PC1"]),
#                    pc2=scale(pca_fit$x[,"PC2"]),
#                    data_yn=iso3%in%file_key$iso3)
#
# # Custom PCA plot
# g <- ggplot(data_pca, mapping=aes(x=pc1, y=pc2, label=iso3, color=data_yn)) +
#   geom_text(size=2) +
#   # Scales
#   labs(x="PC1", y="PC2") +
#   scale_color_manual(name="Data (Y/N)", values=c('grey80', "black")) +
#   # Theme
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))
# g
