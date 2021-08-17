##############################
# Main app
##############################
# TODO: Fix GDP quarterly scaling
# TODO: Fix monthly dates plotting
# TODO: Implement data compares section (one plot)


# Load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
library(plotly)
library(rstudioapi)    
library(stringi)
library(lubridate)


# Set the working directory
loc=rstudioapi::getActiveDocumentContext()$path
loc=str_sub(loc,1,stri_locate_last(loc,fixed='/')[1])
setwd(loc)


# Source functions & data
source("src/functions.R")
source("src/app_data.R")

# Setup UI
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Mockups", disable = FALSE),
  dashboardSidebar(disable = FALSE,
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("th")),
      menuItem("Compare Data", tabName = "compare", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              titlePanel(strong("Data Overview")),
              
              # Row 1 contains inputs - initially loads with values from the first data set
              fluidRow(
                box(
                  # Select type of data to plot 
                  selectInput(inputId = "trendType", label = strong("Trend Type"),
                              choices = get_series_names(),
                              selected = get_series_names()[1]),
                  br(),
                  # Select the frequency of data to plot 
                  radioButtons(inputId = "frequency",
                               label = strong("Frequency"),
                               choices = get_frequency_types(get_series_names()[1])),
                  br(),
                  # Select the date range of data to plot 
                  sliderInput(inputId = "dateSlider",
                              label = strong("Timeframe"),
                              min = get_first_series_date_extremes()[1],
                              max = get_first_series_date_extremes()[2],
                              value = c(get_first_series_date_extremes()[1],
                                        get_first_series_date_extremes()[2]),
                              step = 1,
                              dragRange = TRUE,
                              sep = "")
                )
              ),
              
              # Row 2 contains graphs
              fluidRow(
                box(
                  title = "Plots", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput(outputId = "linePlot"),
                  textOutput(outputId = "plotSource")
                )
              ),
              
              # Row 3 contains tables
              fluidRow(
                box(
                  title = "Tables", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  tableOutput(outputId = "table"),
                  textOutput(outputId = "tableSource")
                )
              )
      ),
      tabItem(tabName = "compare",
              titlePanel(strong("Data Overview"))
      )
    )
  )
)

server <- function(input, output, session) {
  # When the trend type is changed
  observeEvent(input$trendType, {
    # Load in the right radio button options
    updateRadioButtons(session,
                       inputId = "frequency",
                       choices = get_frequency_types(input$trendType))
    
    output$plotSource <- renderText({
      paste0('Source: ', get_series_sources(input$trendType)[2]$SourceName)
    })
    output$tableSource <- renderText({
      paste0('Source: ', get_series_sources(input$trendType)[2]$SourceName)
    })

    # When the frequency radio selection is changed
    observeEvent(input$frequency, {
      # Load in the right data slider range
      updateSliderInput(session,
                       inputId = 'dateSlider',
                       min = year(get_series_date_extremes(input$trendType, input$frequency)[1, 1]),
                       max = year(get_series_date_extremes(input$trendType, input$frequency)[2, 1]),
                       value = c(year(get_series_date_extremes(input$trendType, input$frequency)[1, 1]),
                                 year(get_series_date_extremes(input$trendType, input$frequency)[2, 1])))
      
        # Save currently selected min and max values
      observe({
        val <- input$dateSlider
        # Update plot and table on date slider changed
        observeEvent(input$dateSlider, {
          # Plot
          output$linePlot <- renderPlotly({
            plot <- plot_data(get_series_data(input$trendType, input$frequency),
                              input$dateSlider, input$trendType)
            ggplotly(plot)
          })
          
          # Table
          output$table <- renderTable({
            get_series_data(input$trendType, input$frequency)
          }, striped = TRUE, align = "c")
        })
      })
    })
  })
}

shinyApp(ui = ui, server = server)

