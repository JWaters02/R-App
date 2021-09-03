##############################
# Main app
##############################
# Load libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(readr)
library(tidyverse)
library(plotly)
library(rstudioapi)    
library(stringi)
library(lubridate)


# Set the working directory
loc = rstudioapi::getActiveDocumentContext()$path
loc = str_sub(loc, 1, stri_locate_last(loc, fixed = '/')[1])
setwd(loc)


# Source functions & data
source("src/functions.R")
source("src/app_data.R")


# Setup UI
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Data Visualisation", disable = FALSE),
  dashboardSidebar(disable = FALSE, useShinyjs(),
    sidebarMenu(
      id = "tabs",
      menuItem("Data Overview", tabName = "overview", icon = icon("th")),
      menuItem("Data Summary", tabName = "summary", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              titlePanel(strong("Data Overview")),
              
              # Row 1 contains inputs - initially loads with values from the first data set
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  
                  # Select type of data to plot 
                  selectInput(inputId = "trendType", label = strong("Trend Type"),
                              choices = get_series_names(),
                              selected = get_series_names()[1]),

                  # Select the frequency of data to plot 
                  radioButtons(inputId = "frequency",
                               label = strong("Frequency"),
                               choices = get_frequency_types(get_series_names()[1])),

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
                  title = "Data Plot", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput(outputId = "linePlot"),
                  textOutput(outputId = "plotSource")
                )
              ),
              
              # Row 3 contains tables
              fluidRow(
                box(
                  title = "Data Table", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  tableOutput(outputId = "table"),
                  textOutput(outputId = "tableSource")
                )
              )
      ),
      tabItem(tabName = "summary",
              titlePanel(strong("Data Summary")),
              
              # Row 1 contains inputs
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  
                  # Select the type of trend to show
                  selectInput(inputId = "trendTypeSummary", label = strong("Trend Type"),
                              choices = get_series_names(),
                              selected = get_series_names()[1]),
                  
                  # Select the frequency of data to table
                  radioButtons(inputId = "frequencySummary",
                               label = strong("Frequency"),
                               choices = get_frequency_types(get_series_names()[1]))
                )
              ),
              
              # Row 2 contains plots
              fluidRow(
                box(
                  title = "Summary Box Plot", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput(outputId = "summaryPlot"),
                  textOutput(outputId = "summaryPlotSource")
                )
              ),
              
              # Row 3 contains tables
              fluidRow(
                box(
                  title = "Summary Table", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  tableOutput(outputId = "summaryTable"),
                  textOutput(outputId = "summaryTableSource")
                )
              )
      )
    )
  )
)


# Setup server
server <- function(input, output, session) {
  # DATA OVERVIEW TAB
  # When the trend type is changed
  observeEvent(input$trendType, {
    # Load in the right radio button options
    updateRadioButtons(session,
                       inputId = "frequency",
                       choices = get_frequency_types(input$trendType))
    
    # Output the source text of the trend type
    output$plotSource <- renderText({
      paste0('Source: ', get_series_sources(input$trendType)[2]$SourceName)
    })
    output$tableSource <- renderText({
      paste0('Source: ', get_series_sources(input$trendType)[2]$SourceName)
    })

    # Save currently selected min and max values
    observe({
      val <- input$dateSlider
      
      # Load in the right data slider range
      updateSliderInput(session,
                        inputId = 'dateSlider',
                        min = year(get_series_date_extremes(input$trendType, input$frequency)[1, 1]),
                        max = year(get_series_date_extremes(input$trendType, input$frequency)[2, 1]),
                        value = c(input$dateSlider, input$dateSlider))
      
      # Plot
      output$linePlot <- renderPlotly({
        plot <- plot_line_data(get_series_data(input$trendType, input$frequency),
                               input$dateSlider, input$trendType)
        ggplotly(plot)
      })
      
      # Table
      output$table <- renderTable({
        convert_to_string_date_series_data(input$trendType, input$frequency)
      }, striped = TRUE, align = "c")
    })
  })
  
  
  # SUMMARIES DATA TAB
  # When the trend type is changed
  observeEvent(input$trendTypeSummary, {
    # Load in the right radio button options
    updateRadioButtons(session,
                       inputId = "frequencySummary",
                       choices = get_frequency_types(input$trendTypeSummary))
    
    # Output the source text of the trend type
    output$summaryPlotSource <- renderText({
      paste0('Source: ', get_series_sources(input$trendTypeSummary)[2]$SourceName)
    })
    output$summaryTableSource <- renderText({
      paste0('Source: ', get_series_sources(input$trendTypeSummary)[2]$SourceName)
    })
    
    # When the summary statistic types are changed
    output$summaryText <- renderText({
      summaries <- paste(input$summaryType, collapse = ", ")
      paste("Summaries: ", summaries)
    })
    
    # Plot
    output$summaryPlot <- renderPlotly({
      plot <- plot_box_data(get_series_data(input$trendTypeSummary, input$frequencySummary))
      ggplotly(plot)
    })
    
    # Table
    output$summaryTable <- renderTable({
      generate_summary_dataframe(input$trendTypeSummary, input$frequencySummary)
    }, striped = TRUE, align = "c")
  })
}


# Run app
shinyApp(ui = ui, server = server)