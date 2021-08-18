##############################
# Main app
##############################
# TODO: Fix why compareTypes input condition is always true


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
loc=rstudioapi::getActiveDocumentContext()$path
loc=str_sub(loc,1,stri_locate_last(loc,fixed='/')[1])
setwd(loc)


# Source functions & data
source("src/functions.R")
source("src/app_data.R")

# Setup UI
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Mockups", disable = FALSE),
  dashboardSidebar(disable = FALSE, useShinyjs(),
    sidebarMenu(
      id = "tabs",
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
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  
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
              titlePanel(strong("Compare Data & Summary Statistics")),
              
              # Row 1 contains inputs
              fluidRow(
                box(
                  title = "Inputs", status = "warning", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  
                  # Select whether user wants to show compare or summary stats
                  radioButtons(inputId = "overviewType", label = strong("Overview Type"),
                               choices = c("Compare", "Summary"),
                               selected = "Compare"),
                  
                  # If summary stats is selected
                  conditionalPanel(
                    condition = "input.overviewType == 'Summary'",
                    
                    # Select the type of trend to show
                    selectInput(inputId = "trendTypeSummary", label = strong("Trend Type"),
                                choices = get_series_names(),
                                selected = get_series_names()[1]),
                    
                    # Select the summary type to show
                    checkboxGroupInput(inputId = "summaryType", label = strong("Summaries to Show"),
                                       choices = get_first_summary_statistics())
                  ),
                  
                  # If compare data is selected
                  conditionalPanel(
                    condition = "input.overviewType == 'Compare'",
                    
                    # Select all the trend types to compare
                    checkboxGroupInput(inputId = "compareTypes", label = strong("Trend Types to Compare"),
                                       choices = get_series_names()),
                    
                    # Select the date range of data to plot 
                    sliderInput(inputId = "dateSliderCompare",
                                label = strong("Timeframe"),
                                min = get_first_series_date_extremes()[1],
                                max = get_first_series_date_extremes()[2],
                                value = c(get_first_series_date_extremes()[1],
                                          get_first_series_date_extremes()[2]),
                                step = 1,
                                dragRange = TRUE,
                                sep = "")
                  )
                )
              ),
              
              # Row 2 contains outputs
              fluidRow(id = "outputsRow",
                box(
                  title = "Outputs", status = "success", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  textOutput(outputId = "summaryText")
                )
              ),
              
              # Row 3 contains plots
              fluidRow(
                box(
                  title = "Plots", status = "primary", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  plotlyOutput(outputId = "comparePlot")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # DATA OVERVIEW TAB
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
            plot <- plot_line_data(get_series_data(input$trendType, input$frequency),
                              input$dateSlider, input$trendType, FALSE)
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
  
  
  # COMPARE DATA TAB
  # When the input tab is changed
  observeEvent(input$tabs, {
    observeEvent(input$overviewType, {
      if (input$overviewType == 'Compare') {
        shinyjs::hide(id = "outputsRow")
      } else {
        shinyjs::show(id = "outputsRow")
      }
    })
    
    # When the trend type is changed
    observeEvent(input$trendTypeSummary, {
      # Set up the check box group with values
      updateCheckboxGroupInput(session,
                               inputId = "summaryType",
                               choices = get_summary_statistics(input$trendTypeSummary, '', FALSE)
      )
      
      # When the summary statistic types are changed
      output$summaryText <- renderText({
        summaries <- paste(input$summaryType, collapse = ", ")
        paste("Summaries: ", summaries)
      })
      
      # When the summary checks are changed
      observeEvent(input$compareTypes, {
        print(length(input$compareTypes))
        # Make sure that the number of options entered isn't 0
        if (length(input$compareTypes) >= 1) { # TODO: Fix why condition is always true
          # Plot the graph with the selected types
          output$comparePlot <- renderPlotly({
            plot <- plot_line_data(get_series_data_from_list_no_frequency(input$compareTypes),
                                   input$dateSliderCompare, "Value", TRUE)
            ggplotly(plot)
          })  
        }
      })
    })
  })
}

shinyApp(ui = ui, server = server)

