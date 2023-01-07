library(tidyverse)
library(ggplot2)
library(shiny)
neighbourhood_quoted <- sqldf::read.csv.sql("../data/november_enriched.csv", "SELECT DISTINCT start_neighbourhood as neighbourhood FROM file")
neighbourhood_array <- neighbourhood_quoted%>%
  mutate(neighbourhood = gsub('"', '', neighbourhood))
neighbourhood_array <- as.list(neighbourhood_array)

shinyApp(
  shinyUI(
    navbarPage("Boston Blue Bike stats",
               tabPanel("Inbound", uiOutput('inbound')),
               tabPanel("Outbound", uiOutput('outbound'))
    )
  ),
  shinyServer(function(input, output, session) {
    output$inbound <- renderUI({
      sidebarLayout(
        sidebarPanel(
          selectInput("neighbourhood",
                      "Neighbourhood:",
                      neighbourhood_array,
                      selected = "Roxbury"
          )
        ),


        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("view_inbound")))
    })

    output$outbound <- renderUI({
      sidebarLayout(
        sidebarPanel(
          selectInput("neighbourhood",
                      "Neighbourhood:",
                      neighbourhood_array,
                      selected = "Roxbury"
          )
        ),


        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("view_outbound")))
    })

    november_enriched <- read.csv("../data/november_enriched.csv")
    df_inbound <- reactive({sqldf::sqldf(
      glue::glue("SELECT start_neighbourhood,
  COUNT(*) as journeys
  FROM november_enriched
  WHERE end_neighbourhood = '{input$neighbourhood}'
  GROUP BY 1"))
    })

    output$view_inbound <- renderPlot({
      p <- ggplot(df_inbound(), aes(x=journeys, y=start_neighbourhood)) + geom_bar(stat='identity')
      print(p)
    })

    df_outbound <- reactive({sqldf::sqldf(
      glue::glue("SELECT end_neighbourhood,
  COUNT(*) as journeys
  FROM november_enriched
  WHERE start_neighbourhood = '{input$neighbourhood}'
  GROUP BY 1"))
    })

    output$view_outbound <- renderPlot({
      p <- ggplot(df_outbound(), aes(x=journeys, y=end_neighbourhood)) + geom_bar(stat='identity')
      print(p)
    })
  })
)
