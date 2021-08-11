##############################
# Main app
##############################
# TODO: Fix GDP quarterly scaling
# TODO: Decomp trendTypes into a function
# TODO: Fix table header names
# TODO: Fix sources not changing
# TODO: Fix monthly dates plotting
# TODO: Implement unemployment
# TODO: Implement data compares section (one plot)
# TODO: Remove the need for individual data frame files (e.g. gdp_data.R)


# Load libraries
#library(shiny)
#library(shinydashboard)
#library(readr)
#library(tidyverse)
#library(plotly)


# Source functions & data
#source("src/functions.R")
#source("src/gdp_data.R")
#source("src/cpi_data.R")
#source("src/unemployment_data.R")
#source("src/app_data.R")

# Generate the list of inputs from the total_df
#trend_types_UI <- function(id) {
#  ns <- NS(id)
#  
#  tagList(
#    # Select type of data to plot
#    selectInput(ns("trendType"), label = strong("Trend Type"),
#                choices = get_series_names(),
#                selected = get_series_names()[1]),
#    br(),
#    # Select the frequency of data to plot
#    radioButtons(ns("frequency"),
#                 label = strong("Frequency"),
#                 choices = get_frequency_types(input$trendType)),
#    br(),
#    # Select the date range of data to plot
#    sliderInput(ns("dateSlider"),
#                label = strong("Timeframe"),
#                min = get_series_date_extremes(input$trendType, input$frequency)[1],
#                max = get_series_date_extremes(input$trendType, input$frequency)[2],
#                value = c(get_series_date_extremes(input$trendType, input$frequency)[1],
#                          get_series_date_extremes(input$trendType, input$frequency)[2]),
#                step = 1,
#                dragRange = TRUE,
#                sep = "")
#  )
#}

# Plot GDP values
#plot_gdp <- function(dataframe, isDateQuarterly, limits) {
#  if (isDateQuarterly) {
#    df = dataframe %>%
#      mutate(Year = as.Date(convert_quarterly_dates(Date)))
#  } else {
#    df = dataframe %>%
#      mutate(Year = as.Date(paste0(Date, '-12-31'), "%Y-%m-%d", "GMT"))
#  }
#  colnames(df) = c("Date", "GDP", "Year")
#  ggplot(data = df) +
#    geom_line(aes(x = Year, y = GDP), linetype = "solid", color = "red") +
#    scale_x_date(name = "Years",
#                 limits = as.Date(paste0(limits, '-12-31'), "%Y-%m-%d", "GMT"),
#                 labels = date_format("%Y"))
#}



# Plot CPI values
#plot_cpi <- function(dataframe, dateType, limits) {
#  if (dateType == 'Quarterly') {
#    df = dataframe %>%
#      mutate(Year = as.Date(convert_quarterly_dates(Date)))
#  } else if (dateType == 'Yearly') {
#    df = dataframe %>%
#      mutate(Year = as.Date(paste0(Date, '-12-31'), "%Y-%m-%d", "GMT"))
#  } else if (dateType == 'Monthly') {
#    df = dataframe %>%
#      mutate(Year = as.Date(convert_monthly_dates(Date)))
#  }
#  colnames(df) = c("Date", "Consumer_Price_Index", "Year")
#  ggplot(data = df) +
#    geom_line(aes(x = Year, y = Consumer_Price_Index), linetype = "solid", color = "blue") +
#    scale_x_date(name = "Years",
#                 limits = as.Date(paste0(limits, '-12-31'), "%Y-%m-%d", "GMT"),
#                 labels = date_format("%Y"))
#}


# Setup UI
#ui <- dashboardPage(skin = "red",
#                    dashboardHeader(title = "Mockups", disable = FALSE),
#                    dashboardSidebar(disable = FALSE,
#                                     sidebarMenu(
#                                       menuItem("Data Overview", tabName = "v1", icon = icon("th")),
#                                       menuItem("Compare Data", tabName = "v2", icon = icon("th"))
#                                     )
#                    ),
#                    dashboardBody(
#                      tabItems(
#                        tabItem(tabName = "v1",
#                                titlePanel(strong("Data Overview")),
#                                
#                                # Row 1 contains inputs
#                                fluidRow(
#                                  box(
#                                    title = "Inputs", status = "warning", solidHeader = TRUE,
#                                    width = 12, collapsible = TRUE,
#                                    
#                                    # Select type of data to plot
#                                    selectInput(inputId = "trendType", label = strong("Trend Type"),
#                                                choices = get_series_names(),
#                                                selected = get_series_names()[1]),
#                                    br(),
#                                    
#                                    # If GDP is selected
#                                    conditionalPanel(
#                                      condition = "input.trendType == 'GDP'",
#                                      radioButtons(inputId = "gdpDateType",
#                                                   label = strong("Date Type"),
#                                                   choices = c("Yearly", "Quarterly")),
#                                      br(),
#                                      sliderInput(inputId = "gpdDateSlider",
#                                                  label = strong("Timeframe"),
#                                                  min = gdp_yearly_extremes()[1], max = gdp_yearly_extremes()[2],
#                                                  value = c(gdp_yearly_extremes()[1], gdp_yearly_extremes()[2]),
#                                                  step = 1,
#                                                  dragRange = TRUE,
#                                                  sep = ""),
#                                    ),
#                                    
#                                    # If CPI is selected
#                                    conditionalPanel(
#                                      condition = "input.trendType == 'Consumer Price Index'",
#                                      radioButtons(inputId = "cpiDateType",
#                                                   label = strong("Date Type"),
#                                                   choices = c("Yearly", "Quarterly", "Monthly")),
#                                      br(),
#                                      sliderInput(inputId = "cpiDateSlider",
#                                                  label = strong("Timeframe"),
#                                                  min = cpi_yearly_extremes()[1], max = cpi_yearly_extremes()[2],
#                                                  value = c(cpi_yearly_extremes()[1], cpi_yearly_extremes()[2]),
#                                                  step = 1,
#                                                  dragRange = TRUE,
#                                                  sep = ""),
#                                    )
#                                  )
#                                ),
#                                
#                                # Row 2 contains graphs
#                                fluidRow(
#                                  box(
#                                    title = "Plots", status = "primary", solidHeader = TRUE,
#                                    width = 12, collapsible = TRUE,
#                                    plotlyOutput(outputId = "linePlot"),
#                                    a(href = get_series_sources(input$trendType)$SourceLink,
#                                      "Source: " + get_series_sources(input$trendType)$SourceName)
#                                  )
#                                ),
#                                
#                                # Row 3 contains tables
#                                fluidRow(
#                                  box(
#                                    title = "Tables", status = "success", solidHeader = TRUE,
#                                    width = 12, collapsible = TRUE, collapsed = TRUE,
#                                    tableOutput(outputId = "table"),
#                                    a(href = get_series_sources(input$trendType)$SourceLink,
#                                      "Source: " + get_series_sources(input$trendType)$SourceName)
#                                  )
#                                )
#                       ),
#                        tabItem(tabName = "v2",
#                                tags$h2("Compare Data"),
#                                textInput("txt", "Text input:", "Text"),
#                                actionButton("action", "Button"),
#                                actionButton("action2", "Button2", class = "btn-primary")
#                        )
#                      )
#                    )
#)

#server <- function(input, output, session) {
#  # When the trend type is changed
#  observeEvent(input$trendType, {
#    if (input$trendType == 'GDP') {
#      # When the date type radio selection is changed
#      observeEvent(input$gdpDateType, {
#        # Save currently selected min and max values
#        observe({
#          val <- input$gpdDateSlider
#          
#          # If quarterly selected
#          if (input$gdpDateType == 'Quarterly') {
#            updateSliderInput(session,
#                              inputId = "gpdDateSlider",
#                              min = gdp_quarterly_extremes()[1],
#                              max = gdp_quarterly_extremes()[2],
#                              value = c(val[0], val[1]))
#            
#            # Update quarterly plot and table on date slider changed
#            observeEvent(input$gpdDateSlider, {
#              # Plot
#              output$linePlot <- renderPlotly({
#                plot <- plot_gdp(gdp_quarterly_data(), TRUE, input$gpdDateSlider)
#                ggplotly(plot)
#              })
#             
#              # Table
#              output$table <- renderTable({
#                gdp_quarterly_data()
#              }, striped = TRUE, align = "c")
#            })
#            
#            # If yearly selected
#          } else if (input$gdpDateType == 'Yearly') {
#            # Update yearly plot and table on date slider changed
#            updateSliderInput(session,
#                              inputId = "gpdDateSlider",
#                              min = gdp_yearly_extremes()[1],
#                              max = gdp_yearly_extremes()[2],
#                              value = c(val[0], val[1]))
#            
#            # Update yearly plot and table on date slider changed
#            observeEvent(input$gpdDateSlider, {
#              # Plot
#              output$linePlot <- renderPlotly({
#                plot <- plot_gdp(gdp_yearly_data(), FALSE, input$gpdDateSlider)
#                ggplotly(plot)
#              })
#              
#              # Table
#              output$table <- renderTable({
#                gdp_yearly_data()
#              }, striped = TRUE, align = "c")
#            })
#          }
#        })
#      })
#      
#      # If CPI selected  
#    } else if (input$trendType == 'Consumer Price Index') {
#      # When the date type radio selection is changed
#      observeEvent(input$cpiDateType, {
#        # Save currently selected min and max values
#        observe({
#          val <- input$cpiDateSlider
#          
#          # If quarterly selected
#          if (input$cpiDateType == 'Quarterly') {
#            updateSliderInput(session,
#                              inputId = "cpiDateSlider",
#                              min = cpi_quarterly_extremes()[1],
#                              max = cpi_quarterly_extremes()[2],
#                              value = c(val[0], val[1]))
#            
#            # Update quarterly plot and table on date slider changed
#            observeEvent(input$cpiDateSlider, {
#              # Plot
#              output$linePlot <- renderPlotly({
#                plot <- plot_cpi(cpi_quarterly_data(), "Quarterly", input$cpiDateSlider)
#                ggplotly(plot)
#              })
#              
#              # Table
#              output$table <- renderTable({
#                cpi_quarterly_data()
#             }, striped = TRUE, align = "c")
#            })
#            
#            # If yearly selected
#          } else if (input$cpiDateType == 'Yearly') {
#            # Update yearly plot and table on date slider changed
#            updateSliderInput(session,
#                              inputId = "cpiDateSlider",
#                              min = cpi_yearly_extremes()[1],
#                              max = cpi_yearly_extremes()[2],
#                              value = c(val[0], val[1]))
#            
#            # Update yearly plot and table on date slider changed
#            observeEvent(input$cpiDateSlider, {
#              # Plot
#              output$linePlot <- renderPlotly({
#                plot <- plot_cpi(cpi_yearly_data(), "Yearly", input$cpiDateSlider)
#                ggplotly(plot)
#              })
#              
#              # Table
#              output$table <- renderTable({
#                cpi_yearly_data()
#              }, striped = TRUE, align = "c")
#            })
#           
#            # If monthly selected  
#          } else {
#            # Update yearly plot and table on date slider changed
#            updateSliderInput(session,
#                              inputId = "cpiDateSlider",
#                              min = cpi_monthly_extremes()[1],
#                              max = cpi_monthly_extremes()[2],
#                              value = c(val[0], val[1]))
#            
#           # Update yearly plot and table on date slider changed
#           observeEvent(input$cpiDateSlider, {
#              # Plot
#              output$linePlot <- renderPlotly({
#                plot <- plot_cpi(cpi_monthly_data(), "Monthly", input$cpiDateSlider)
#                ggplotly(plot)
#              })
#              
#              # Table
#              output$table <- renderTable({
#               cpi_monthly_data()
#              }, striped = TRUE, align = "c")
#            })
#          }
#        })
#      })
#      
#      # If unemployment selected  
#    } else if (input$trendType == 'Unemployment') {
#      
#    }
#  })
#}

#shinyApp(ui = ui, server = server)