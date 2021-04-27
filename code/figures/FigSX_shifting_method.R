

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(fitdistrplus)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational_distributions/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"
tabledir <- "tables"

# Read distribution key
dists <- readRDS(file.path(datadir, "nutrient_intake_distributions_21countries.Rds")) %>%
  filter(best_dist!="none")


# Plot data
################################################################################

# Iron
dris <- nutriR::dris
iron_dri <- dris %>%
  filter(nutrient=="Iron" & sex=="Females" & age_range=="19-30 yr" & stage=="None" & dri_type=="Estimated Average Requirement (EAR)")

# Read example data
gamma_ex <- dists %>%
  filter(country=="Bangladesh" & nutrient=="Iron" & sex=="Females" & age_group=="20-24")

# Parameters
shape <- gamma_ex$g_shape
rate <- gamma_ex$g_rate
mean1 <- shape/rate

# Shift
shift1 <- 3
mean2 <- mean1 + shift1
scalar1 <- mean2 / mean1

# Shift
shift2 <- -1
mean3 <- mean1 + shift2
scalar2 <- mean3 / mean1

# Simulations
xmax <- qgamma(p=0.9999, shape=shape, rate=rate)
x <- seq(0, xmax, length.out = 200)
y1 <- dgamma(x, shape=shape, rate=rate)
y2 <- dgamma(x, shape=shape, rate=rate/scalar1)
y3 <- dgamma(x, shape=shape, rate=rate/scalar2)
plot(x, y1)
points(x, y2, col="blue")
points(x, y3, col="red")

# Create dataframe
data <- tibble(intake=x, y1, y2, y3) %>%
  gather(key="scenario", value="density", 2:ncol(.)) %>%
  mutate(scenario=recode_factor(scenario,
                                "y1"="Current",
                                "y2"="+3 mg/day",
                                "y3"="-1 mg/day"))


# Plot data
g <- ggplot(data, aes(x=intake, y=density, color=scenario)) +
  geom_vline(xintercept=iron_dri$value, color="grey50", linetype="dotted", lwd=0.5) +
  # Lines
  geom_line(lwd=0.8) +
  # Labels
  labs(x="Habitual intake (mg/d)", y="Density") +
  scale_color_manual(name="Distribution", values=c("black", "blue", "red")) +
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
ggsave(g, filename=file.path(plotdir, "FigSX_shifting_method_gamma.png"),
       width=3.5, height=3.5, units="in", dpi=600)



