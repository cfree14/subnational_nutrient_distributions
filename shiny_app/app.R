
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
# datadir <- "shiny/data" # when testing
# codedir <- "shiny/code" # when testing

# Source code
sapply(list.files(codedir), function(x) source(file.path(codedir, x)))

# Read data
dris <-nutriR::dris
dists_full <- nutriR::dists_full %>%
  filter(sex!="Children")



# Parameters
################################################################################

# Nutrients
nutrients <- sort(unique(dists_full$nutrient))

# Base theme
base_theme <- theme(axis.text=element_text(size=14),
                    axis.title=element_text(size=16),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=16),
                    strip.text=element_text(size=16),
                    plot.subtitle=element_text(size=18),
                    plot.title=element_text(size=16),
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
     plotOutput(outputId = "plot_intake_dists", width=600, height=2000),
     br(),

     # Illustrate means
     h3("Subnational habitual intake means"),
     plotOutput(outputId = "plot_intake_means", width=600, height=2000),
     br(),

     # Illustrate prevalence of inadequate intakes
     h3("Prevalence of inadequate intakes"),
     plotOutput(outputId = "plot_inadequate_intakes", width=1000, height=600),
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

  # Plot intake means
  output$plot_intake_dists <- renderPlot({
    g <- plot_intake_dists(data = dists_full,
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
