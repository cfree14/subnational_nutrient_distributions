
# Clear
rm(list = ls())
options(dplyr.summarise.inform=F)

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
# dists_full_orig <- nutriR::dists_full # when nutriR is finalized
dists_full_orig <- readRDS(file=file.path(datadir, "nutrient_intake_distributions_23countries_expanded.Rds"))

# Format distributions
dists_full <- dists_full_orig %>%
  # Remove un-fit distributions
  filter(best_dist!="none") %>%
  # Remove children
  filter(sex!="Children") %>%
  # Remove problem countries
  # filter(!country %in% c("Philippines")) %>% # "Burkina Faso"
  # Recode countries for plotting
  mutate(country_label=recode(country,
                              "Bosnia & Herzegovina"="Bosnia &\nHerzegovina"))

# Read overlaps
overlaps_orig <- readRDS(file.path(datadir, "percent_overlap_among_country_pairs.Rds"))

# Format overlaps
overlaps <- overlaps_orig %>%
  # Remove Philippines
  # filter(iso1!="PHL" & iso2!="PHL") %>%
  # Calculate average
  group_by(nutrient, sex, age) %>%
  summarize(overlap=mean(poverlap)) %>%
  ungroup() %>%
  # Format
  rename(age_group=age) %>%
  mutate(age_group=factor(age_group, levels=levels(dists_full$age_group)))


# Parameters
################################################################################

#  Parameters
countries <- sort(unique(dists_full$country))
nutrients <- sort(unique(dists_full$nutrient))
cntry_nutr_key <- dists_full %>%
  select(country, nutrient) %>%
  unique() %>%
  arrange(country, nutrient)

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
     p("The figure below illustrates the availability of subnational intake distributions for the selected nutrient by country, sex, and age group."),
     plotOutput(outputId = "plot_coverage", width=600, height=375),
     br(),

     # Illustrate distributions
     h3("Subnational habitual intake distributions"),
     p("The figure below illustrates habitual intake distributions by sex and age group within each country. The vertical black lines indicate the median EARs for men and women across availbale age groups (if an EAR is available). The vertical red lines indicate the median ULs for men and women across availbale age groups (if a UL is available)."),
     radioButtons(inputId = "scales1", label="X-axis scale:", choices = c("Fixed", "Free"), selected = "Fixed", inline=T),
     radioButtons(inputId = "ul_yn1", label="Show upper limit (UL)?:", choices = c("Yes", "No"), selected = "Yes", inline=T),
     plotOutput(outputId = "plot_intake_dists", width=800, height=1000),
     br(),

     # Illustrate distributions - difference among countries
     h3("Similarity in subnational habitual intake distributions across countries"),
     p("In the figure below, the similarity in habitual intake distributions across countries are compared within sex and age groups. The similarity is quantified as the mean percent overlap of all pairwise combinations of countries with available data. The percent overlap is calculated as the Bhattacharyya coefficient. The solid vertical lines indicate the EAR for the sex-age group, if an EAR is available."),
     radioButtons(inputId = "scales2", label="X-axis scale:", choices = c("Fixed", "Free"), selected = "Fixed", inline=T),
     radioButtons(inputId = "ul_yn2", label="Show upper limit (UL)?:", choices = c("Yes", "No"), selected = "Yes", inline=T),
     plotOutput(outputId = "plot_intake_dists_age_group", width=1000, height=1200),
     br(),

     # Illustrate means
     h3("Subnational habitual intake means"),
     p("The figure below illustrate the mean habitual intake for each country, sex, and age group compared to the Estimated Average Requirement (EAR), when available. The EAR is from the U.S. Food and Nutrition Board of the National Academy of Sciences."),
     radioButtons(inputId = "scales3", label="X-axis scale:", choices = c("Fixed", "Free"), selected = "Fixed", inline=T),
     radioButtons(inputId = "ul_yn3", label="Show upper limit (UL)?:", choices = c("Yes", "No"), selected = "Yes", inline=T),
     plotOutput(outputId = "plot_intake_means", width=600, height=2000),
     br(),

     # Illustrate prevalence of inadequate intakes
     h3("Prevalence of inadequate intakes"),
     p("The figure below illustrate the prevalence of inadequate intakes estimated using the probability method."),
     plotOutput(outputId = "plot_inadequate_intakes", width=600, height=375),
     br()

  ),

  # Explore by country
  tabPanel("Explore by country",

     # Select by nutrient
     selectInput(inputId = "country", label = "Select a country:",
                 choices = countries,  multiple = F, selected="United States"),
     br(),

     # Illustrate coverage
     h3("Data coverage"),
     p("The figure below illustrates the availability of subnational intake distributions for the selected country."),
     plotOutput(outputId = "plot_coverage_by_country", width=600, height=600),
     br(),

     # Illustrate distribution
     h3("Habitual intake distributions"),
     p("The figure belows shows habitual intake distributions ny nutrient, sex, and age within the selected country. The vertical lines indicate the EAR, if available."),
     radioButtons(inputId = "ul_yn4", label="Show upper limit (UL)?:", choices = c("Yes", "No"), selected = "Yes", inline=T),
     plotOutput(outputId = "plot_intake_dists_cntry", width=800, height=1600),
     br(),

     # # Select a nutrient
     # selectInput(inputId = "nutrient2", label = "Select a nutrient\n(only nutrients with data are listed):",
     #             choices = nutrients,  multiple = F, selected="Calcium"),
     # br(),
     #
     # # Illustrate SPADE ouput and fits
     # h3("SPADE output and distribution fits"),
     # p("In the figure below, the shading indicates the distributions of habitual intakes estimated by SPADE and the lines indicate the distribution fit to this data."),
     # plotOutput(outputId = "plot_fits_over_obs", width=800, height=800),
     # br(),

     # Prevalence of inadequate intakes
     h3("Prevalence of inadequate intakes"),
     p("The figure below indicates the prevalence on inadequate nutrient intakes within the selected country by sex and age group."),
     plotOutput(outputId = "plot_inadequate_intakes_in_a_country", width=800, height=800),
     br(),

     # Prevalence of inadequate intakes
     h3("Prevalence of intakes over the upper limit"),
     p("The figure below indicates the prevalence of intakes over the upper limit within the selected country by sex and age group."),
     plotOutput(outputId = "plot_overage_intakes_in_a_country", width=800, height=400),
     br()

  )

)


# Server
################################################################################

# Server
server <- function(input, output, session){

  # By nutrient
  ###########################

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
                           scales = input$scales1,
                           ul_yn = input$ul_yn1,
                           base_theme = base_theme)
    g
  })

  # Plot intake distributions
  output$plot_intake_dists_age_group <- renderPlot({
    g <- plot_intake_dists_age_group(data = dists_full,
                                     nutrient = input$nutrient,
                                     overlaps = overlaps,
                                     scales = input$scales2,
                                     ul_yn = input$ul_yn2,
                                     base_theme = base_theme)
    g
  })

  # Plot intake means
  output$plot_intake_means <- renderPlot({
    g <- plot_intake_means(data = dists_full,
                           nutrient = input$nutrient,
                           scales = input$scales3,
                           ul_yn = input$ul_yn3,
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

  # By country
  ###########################

  # Select nutrients based on country availability
  observeEvent(input$country, {
    country_do <- input$country
    nutrients2 <- cntry_nutr_key$nutrient[cntry_nutr_key$country==country_do ]
    updateSelectInput(session, "nutrient2", choices=nutrients2)
  })

  # Plot coverage
  output$plot_coverage_by_country <- renderPlot({
    g <- plot_coverage_by_country(data = dists_full,
                                  country = input$country,
                                  base_theme = base_theme)
    g
  })

  # Plot inadequate intakes eithin a country
  output$plot_intake_dists_cntry <- renderPlot({
    g <- plot_intake_dists_cntry(data = dists_full,
                                 country = input$country,
                                 ul_yn = input$ul_yn4,
                                 base_theme = base_theme)
    g
  })

  # # Plot  SPADE ouput and fits
  # output$plot_fits_over_obs <- renderPlot({
  #   g <- plot_fits_over_obs(data = dists_full,
  #                           nutrient = input$nutrient2,
  #                           country = input$country,
  #                           base_theme = base_theme,
  #                           datadir = datadir)
  #   g
  # })

  # Plot inadequate intakes within a country
  output$plot_inadequate_intakes_in_a_country <- renderPlot({
    g <- plot_inadequate_intakes_in_a_country(data = dists_full,
                                 country = input$country,
                                 base_theme = base_theme)
    g
  })

  # Plot overage intakes within a country
  output$plot_overage_intakes_in_a_country <- renderPlot({
    g <- plot_overage_intakes_in_a_country(data = dists_full,
                                           country = input$country,
                                           base_theme = base_theme)
    g
  })


}

shinyApp(ui = ui, server = server)
