

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read distribution key
dists <- readRDS(file.path(datadir, "nutrient_intake_distributions_31countries.Rds")) %>%
  filter(best_dist!="none")


# Plot data
################################################################################

# Read example data
data_orig <- read.csv(file.path(inputdir, "bang_w_iron.csv"), as.is=T)

# Format data
data <- data_orig %>%
  # Rename
  rename(id=X, age_yr=age, intake=HI) %>%
  # Filter to age group
  mutate(age_group=cut(age_yr, breaks=c(seq(0,100,5)))) %>%
  filter(age_group=="(20,25]")

# Fit distributions
gamma_fit <- fitdist(data$intake, distr="gamma")
lognorm_fit <- fitdist(data$intake, distr="lnorm")
coef(gamma_fit )
coef(lognorm_fit )
gofstat(list(gamma_fit, lognorm_fit))

# Simulate distributions
xmax <- max(data$intake)
x <- seq(0, xmax, length.out = 200)
ygamma <- dgamma(x, shape=coef(gamma_fit)["shape"], rate=coef(gamma_fit)["rate"])
ylognorm <- dlnorm(x, meanlog=coef(lognorm_fit)["meanlog"], sdlog=coef(lognorm_fit)["sdlog"])
df <- tibble(intake=x, ygamma=ygamma, ylognorm=ylognorm) %>%
  gather(key="distribution", value="density", 2:ncol(.)) %>%
  mutate(distribution=recode(distribution,
                             "ygamma"="Gamma",
                             "ylognorm"="Log-normal"))

# Plot data
g <- ggplot(data, aes(x=intake)) +
  geom_density(fill="grey80", color="grey50", lwd=0.2) +
  # Plot fits
  geom_line(data=df, mapping=aes(x=intake, y=density, color=distribution)) +
  # Labels
  labs(x="Usual intake (mg/d)", y="Density") +
  scale_color_manual(name="Distribution", values=c("darkred", "navy")) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
              axis.title=element_text(size=8),
              legend.text=element_text(size=6),
              legend.title=element_text(size=8),
              strip.text=element_text(size=8),
              plot.title=element_blank(),
              # Gridlines
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              # Legend
              legend.position=c(0.8, 0.8))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS4_fitting_procedure_example.png"),
       width=3.5, height=3.5, units="in", dpi=600)



