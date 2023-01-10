library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)

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
          ),
          selectInput("statistic_inbound",
                      "Travel statistic:",
                      c("Travel time" = "time",
                        "Total journeys" = "journeys"),
                      selected = "Travel time"
          )
        ),


        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("view_inbound"),
          leafletOutput("leaflet_inbound")))
    })

    output$outbound <- renderUI({
      sidebarLayout(
        sidebarPanel(
          selectInput("neighbourhood",
                      "Neighbourhood:",
                      neighbourhood_array,
                      selected = "Roxbury"
          ),
          selectInput("statistic_outbound",
                      "Travel statistic:",
                      c("Travel time" = "time",
                        "Total journeys" = "journeys"),
                      selected = "Travel time"
          )
        ),


        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("view_outbound"),
          leafletOutput("leaflet_outbound")))
    })

    november_enriched <- read.csv("../data/november_enriched.csv")
    df_inbound <- reactive({sqldf::read.csv.sql("../data/november_enriched.csv",
                                                glue::glue("SELECT start_neighbourhood,
  COUNT(*) as journeys
  FROM file
  WHERE end_neighbourhood = '\"{input$neighbourhood}\"'
  GROUP BY 1"))%>%mutate(start_neighbourhood = gsub('"', '', start_neighbourhood))
    })

    output$view_inbound <- renderPlot({
      p <- ggplot(df_inbound(), aes(x=journeys, y=reorder(start_neighbourhood, -journeys))) + geom_bar(stat='identity')
      print(p)
    })

    leaflet_outbound <- reactive({BostonBikes::leaflet_maker(input$statistic_outbound, 'outbound', geojson_path = "../data/Boston_Neighborhoods.geojson")})

    leaflet_inbound <- reactive({BostonBikes::leaflet_maker(input$statistic_inbound, 'inbound', geojson_path = "../data/Boston_Neighborhoods.geojson")})


    df_outbound <- reactive({sqldf::read.csv.sql("../data/november_enriched.csv",
                                                 glue::glue("SELECT end_neighbourhood,
    COUNT(*) as journeys
    FROM file
    WHERE start_neighbourhood = '\"{input$neighbourhood}\"'GROUP BY 1"))%>%mutate(end_neighbourhood = gsub('"', '', end_neighbourhood))
    })

    output$view_outbound <- renderPlot({
      p <- ggplot(df_outbound(), aes(x=journeys, y=reorder(end_neighbourhood, -journeys))) + geom_bar(stat='identity')
      print(p)
    })

    output$leaflet_inbound <- renderLeaflet({leaflet_inbound()})
    output$leaflet_outbound <- renderLeaflet({leaflet_outbound()})
  })
)
