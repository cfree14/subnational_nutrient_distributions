
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(nutriR)
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data" # for actual app
codedir <- "code"  # for actual app
# datadir <- "shiny_app/data" # when testing
# codedir <- "shiny_app/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read DRIS
dris <-nutriR::dris

# Read distributions
dists_full <- nutriR::dists_full %>%
  # Remove children
  filter(sex!="Children") %>%
  # Remove problem countries
  filter(!country %in% c("Philippines")) %>% # "Burkina Faso"
  # Recode countries for plotting
  mutate(country_label=recode(country,
                              "Bosnia & Herzegovina"="Bosnia &\nHerzegovina"))

# Read overlaps
overlaps_orig <- readRDS(file.path(datadir, "percent_overlap_among_country_pairs.Rds"))

# Format overlaps
overlaps <- overlaps_orig %>%
  # Remove Philippines
  filter(iso1!="PHL" & iso2!="PHL") %>%
  # Calculate average
  group_by(nutrient, sex, age) %>%
  summarize(overlap=mean(poverlap)) %>%
  ungroup() %>%
  # Format
  rename(age_group=age) %>%
  mutate(age_group=factor(age_group, levels=levels(dists_full$age_group)))

# Parameters
################################################################################

# Nutrients
nutrients <- sort(unique(dists_full$nutrient))

# Base theme
base_theme <- theme(axis.text=element_text(size=12),
                    axis.title=element_text(size=14),
                    legend.text=element_text(size=12),
                    legend.title=element_text(size=14),
                    strip.text=element_text(size=14),
                    plot.subtitle=element_text(size=12),
                    plot.title=element_text(size=14),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


# User interface
################################################################################

# User interface
ui <- navbarPage("Subnational nutrient intake distribution explorer",

  # Explore by nutrient
  tabPanel("Explore by nutrient",

     # Select by nutrient
     selectInput(inputId = "nutrient", label = "Select a nutrient:",
                 choices = nutrients,  multiple = F, selected="Iron"),
     br(),

     # Illustrate coverage
     h3("Data coverage"),
     plotOutput(outputId = "plot_coverage", width=600, height=375),
     br(),

     # Illustrate distributions
     h3("Subnational habitual intake distributions"),
     plotOutput(outputId = "plot_intake_dists", width=600, height=1000),
     br(),

     # Illustrate distributions - difference among countries
     h3("Subnational habitual intake distributions - diff age groups"),
     plotOutput(outputId = "plot_intake_dists_age_group", width=1000, height=1200),
     br(),

     # Illustrate means
     h3("Subnational habitual intake means"),
     plotOutput(outputId = "plot_intake_means", width=600, height=2000),
     br(),

     # Illustrate prevalence of inadequate intakes
     h3("Prevalence of inadequate intakes"),
     plotOutput(outputId = "plot_inadequate_intakes", width=600, height=375),
     br()

  ),

  # Explore by country
  tabPanel("Explore by country",
  )

)


# Server
################################################################################

# Server
server <- function(input, output){

  # Plot coverage
  output$plot_coverage <- renderPlot({
    g <- plot_coverage(data = dists_full,
                       nutrient = input$nutrient,
                       base_theme = base_theme)
    g
  })

  # Plot intake distributions
  output$plot_intake_dists <- renderPlot({
    g <- plot_intake_dists(data = dists_full,
                           nutrient = input$nutrient,
                           base_theme = base_theme)
    g
  })

  # Plot intake distributions
  output$plot_intake_dists_age_group <- renderPlot({
    g <- plot_intake_dists_age_group(data = dists_full,
                                     nutrient = input$nutrient,
                                     base_theme = base_theme)
    g
  })

  # Plot intake means
  output$plot_intake_means <- renderPlot({
    g <- plot_intake_means(data = dists_full,
                           nutrient = input$nutrient,
                           base_theme = base_theme)
    g
  })

  # Plot inadequate intakes
  output$plot_inadequate_intakes <- renderPlot({
    g <- plot_inadequate_intakes(data = dists_full,
                                 nutrient = input$nutrient,
                                 base_theme = base_theme)
    g
  })


}

shinyApp(ui = ui, server = server)
