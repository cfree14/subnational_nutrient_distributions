

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(nutriR)
library(tidyverse)

# Directories
plotdir <- "figures"
tabledir <- "tables"


# Build data
################################################################################

# Distribution mean and requirement
mu <- 10
ear <- 12
ear_cv <- 0.1

# Scenarios
cv <- 0.4
cv_lo <- 0.2
cv_hi <- 0.6

# Normal distribution
norm_mu <- mu
norm_sd <- norm_mu * cv
x <- seq(0.1, 25, 0.1)
norm_y <- dnorm(x=x, mean=norm_mu, sd=norm_sd)
# plot(norm_y~x)
norm_sev <- round(nutriR::cutpoint(ear=ear, intake_avg = norm_mu, intake_cv=cv, intake_dist="normal"), 0)
norm_label <- paste0("Normal (", norm_sev, "%)")
norm_df <- tibble(dist=norm_label,
                  x=x,
                  y=norm_y)
norm_mu_dens <- dnorm(x=norm_mu, mean=norm_mu, sd=norm_sd)

# Log-normal distribution (same CV)
lnorm_base_sdlog <- sqrt(log(cv^2+1))
lnorm_base_meanlog <- log(mu) - lnorm_base_sdlog^2/2
lnorm_base_y <- dlnorm(x=x, meanlog=lnorm_base_meanlog, sdlog=lnorm_base_sdlog)
# plot(lnorm_y~x)
lnorm_base_sev <- nutriR::sev(ear=ear, cv=ear_cv, meanlog=lnorm_base_meanlog, sdlog=lnorm_base_sdlog) %>% round(., 0)
lnorm_base_label <- paste0("Log-normal (", lnorm_base_sev, "%)")
lnorm_base_df <- tibble(dist=lnorm_base_label,
                   x=x,
                   y=lnorm_base_y)
nutriR::mean_dist(meanlog=lnorm_base_meanlog, sdlog=lnorm_base_sdlog) # check mean is right

# Gamma
gamma_alpha <- (1/cv)^2
gamma_beta <- gamma_alpha / mu
gamma_y <- dgamma(x=x, shape=gamma_alpha, rate=gamma_beta)
# plot(gamma_y~x)
gamma_sev <- nutriR::sev(ear=ear, cv=ear_cv, shape=gamma_alpha, rate=gamma_beta) %>% round(., 0)
gamma_label <- paste0("Gamma (", gamma_sev, "%)")
gamma_df <- tibble(dist=gamma_label,
                        x=x,
                        y=gamma_y)
nutriR::mean_dist(shape=gamma_alpha, rate=gamma_beta) # check mean is right

# First plot dataset
########################################

# Merge data
df1 <- bind_rows(norm_df, lnorm_base_df, gamma_df) %>%
  mutate(dist=factor(dist, levels=c(norm_label, gamma_label, lnorm_base_label)))

# Second plot dataset
########################################

# Base
lnorm_cv_base_label <- gsub("Log-normal", "Moderate variability", lnorm_base_label)
lnorm_cv_base_df <- lnorm_base_df %>%
  mutate(dist=lnorm_cv_base_label)

# Log-normal distribution (low CV)
sdlog_cv_lo <- sqrt(log(cv_lo^2+1))
meanlog_cv_lo <- log(mu) - sdlog_cv_lo^2/2
lnorm_cv_lo_y <- dlnorm(x=x, meanlog=meanlog_cv_lo, sdlog=sdlog_cv_lo)
# plot(lnorm_y~x)
lnorm_cv_lo_sev <- nutriR::sev(ear=ear, cv=ear_cv, meanlog=meanlog_cv_lo, sdlog=sdlog_cv_lo) %>% round(., 0)
lnorm_cv_lo_label <- paste0("Low variability (", lnorm_cv_lo_sev, "%)")
lnorm_cv_lo_df <- tibble(dist=lnorm_cv_lo_label,
                   x=x,
                   y=lnorm_cv_lo_y)
nutriR::mean_dist(meanlog=meanlog_cv_lo, sdlog=sdlog_cv_lo) # check mean is right
lnorm_cv_lo_mu_dens <- dlnorm(x=mu, meanlog=meanlog_cv_lo, sdlog=sdlog_cv_lo)

# Log-normal distribution (high CV)
sdlog_cv_hi <- sqrt(log(cv_hi^2+1))
meanlog_cv_hi <- log(mu) - sdlog_cv_hi^2/2
lnorm_cv_hi_y <- dlnorm(x=x, meanlog=meanlog_cv_hi, sdlog=sdlog_cv_hi)
# plot(lnorm_y~x)
lnorm_cv_hi_sev <- nutriR::sev(ear=ear, cv=ear_cv, meanlog=meanlog_cv_hi, sdlog=sdlog_cv_hi) %>% round(., 0)
lnorm_cv_hi_label <- paste0("High variability (", lnorm_cv_hi_sev, "%)")
lnorm_cv_hi_df <- tibble(dist=lnorm_cv_hi_label,
                         x=x,
                         y=lnorm_cv_hi_y)
nutriR::mean_dist(meanlog=meanlog_cv_hi, sdlog=sdlog_cv_hi) # check mean is right

# Merge data
df2 <- bind_rows(lnorm_cv_lo_df, lnorm_cv_base_df, lnorm_cv_hi_df) %>%
  mutate(dist=factor(dist, levels=c(lnorm_cv_lo_label, lnorm_cv_base_label, lnorm_cv_hi_label)))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=6),
                    plot.tag = element_text(size=8),
                    plot.subtitle = element_text(size=7),
                    plot.title=element_text(size=8, face="bold"),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    legend.background = element_rect(fill=alpha('blue', 0)),
                    legend.key.size = unit(0.25, "cm"))

# A) Normal vs. skewed
g1 <- ggplot(df1, aes(x=x, y=y, color=dist)) +
  geom_line() +
  # Plot EAR
  geom_vline(xintercept = ear) +
  # Plot mean
  geom_vline(xintercept=mu, linetype="dotted", color="black") +
  # Labels
  labs(x="Usual intake (mg)", y='Density', tag="A",
       title="Distribution matters",
       subtitle="Same mean and variability but different distribution") +
  scale_color_discrete(name="Distribution\n(% inadequate intake)") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.75, 0.8))
g1

# B) Same mean, greater variation (and skewness)
g2 <- ggplot(df2, aes(x=x, y=y, color=dist)) +
  geom_line() +
  # Plot EAR
  geom_vline(xintercept = ear) +
  # Plot mean
  geom_vline(xintercept=mu, linetype="dotted", color="black") +
  # Labels
  labs(x="Usual intake (mg)", y='Density', tag="B",
       title="Variability matters",
       subtitle="Same mean and distribution but different variability") +
  scale_color_discrete(name="Variability\n(% inadequate intake)") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.75, 0.8))
g2


# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_stylized_examples.png"),
       width=6.5, height=2.5, units="in", dpi=600)


