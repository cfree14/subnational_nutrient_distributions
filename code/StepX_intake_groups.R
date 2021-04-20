

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(ggbiplot)


# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
outdir <- "data/temp"
plotdir <- "figures"

# Read file key
file_key <- read.csv(file=file.path(datadir, "SPADE_file_key.csv"), as.is=T)

# Read GENUS data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/nutrient_endowment/data/genus/processed/genus_edible_food_by_cntry_year.Rds")

# Read map
world <- rnaturalearth::ne_countries(scale="small", returnclass="sf")


# PCA play
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
  replace(is.na(.), 0)

# Convert to matrix
data_mat <- data %>%
  select(-c(iso3, country)) %>%
  as.matrix()
rownames(data_mat) <- data$iso3

# Fit PCA
pca_fit <- prcomp(data_mat)

# Plot PCA data
ggbiplot(pca_fit, labels=rownames(data_mat), var.axes=F) +
  theme_bw()

# Extract PCA info for plotting
data_pca <- tibble(iso3=rownames(data_mat),
                   pc1=scale(pca_fit$x[,"PC1"]),
                   pc2=scale(pca_fit$x[,"PC2"]),
                   data_yn=iso3%in%file_key$iso3)

# Custom PCA plot
g <- ggplot(data_pca, mapping=aes(x=pc1, y=pc2, label=iso3, color=data_yn)) +
  geom_text(size=2) +
  # Scales
  labs(x="PC1", y="PC2") +
  scale_color_manual(name="Data (Y/N)", values=c('grey80', "black")) +
  # Theme
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
g



# K means
################################################################################

# https://compgenomr.github.io/book/clustering-grouping-samples-based-on-their-similarity.html#k-means-clustering

# K-means using factoextra
# https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/

# Package
library(factoextra)

# Fit k-means using maximum possible groups given data (21 countries)
kmeans_fit <- eclust(data_mat, "kmeans", k.max=21)

# Inspect gap statistic (optimal k choice?)
kmeans_fit$nbclust
fviz_gap_stat(kmeans_fit$gap_stat)

# Inspect silhouette
fviz_silhouette(kmeans_fit)

# Extract cluster assignments
# str(kmeans_fit)
cluster_vec <- kmeans_fit$cluster
cluster_df <- kmeans_fit$clust_plot$data %>%
  rename(iso3=name)

# Add to world and plot
world1 <- world %>%
  left_join(cluster_df, by=c("gu_a3"="iso3"))

# Plot
g <- ggplot() +
  geom_sf(world1, mapping=aes(fill=cluster)) +
  theme_bw()
g

# Hierarchical clustering
################################################################################

# Compute hierarchical clustering
hc_fit <- eclust(data_mat, "hclust", k.max=7)

# Inspect dendrogram
fviz_dend(hc_fit, rect = TRUE) # dendrogam

# Inspect gap
fviz_gap_stat(hc_fit$gap_stat)

# Inspect grouping
fviz_cluster(hc_fit)


# Extract cluster assignments
# str(kmeans_fit)
cluster_vec <- hc_fit$cluster
cluster_df <- tibble(iso3=names(cluster_vec),
                     cluster=cluster_vec %>% as.character())

# Add to world and plot
world2 <- world %>%
  left_join(cluster_df, by=c("gu_a3"="iso3"))

# Plot
g <- ggplot() +
  geom_sf(world2, mapping=aes(fill=cluster)) +
  theme_bw()
g



