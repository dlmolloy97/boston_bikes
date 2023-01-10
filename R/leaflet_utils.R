#' @export
#' This function extracts latest zipfile for user data
query_engine <- function(statistic, direction) {
  if (direction == 'inbound') {
    grouping <- 'start_neighbourhood'
  } else if (direction == 'outbound') {
    grouping <- 'start_neighbourhood'}

  if (statistic == 'time') {
    query <- glue::glue("SELECT {grouping}, AVG(tripduration) as journey_time from file group by 1")
  } else if (statistic == 'journeys') {
    query <- glue::glue("SELECT {grouping}, COUNT(*) as journeys from file group by 1")
  }

    journeys <- sqldf::read.csv.sql("../data/november_enriched.csv", "SELECT start_neighbourhood, COUNT(*) as journeys from file group by 1")%>%
      mutate(start_neighbourhood = gsub('"', '', start_neighbourhood))
    return(journeys)


}

#' @export
#' This function extracts latest zipfile for user data

leaflet_maker <- function(statistic, direction, geojson_path = "../data/Boston_Neighborhoods.geojson") {
  boston_districts <- rgdal::readOGR(geojson_path)
  journeys <- BostonBikes::query_engine(statistic, direction)
  boston_districts@data <- boston_districts@data %>%
    dplyr::left_join(journeys, by = c("Name" = "start_neighbourhood"))

  pal <- colorNumeric("viridis", NULL)


  leaflet_map <- leaflet(boston_districts) %>%
    addTiles()%>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.6,
                fillColor = ~pal(boston_districts@data$journeys))%>%
    addLegend(pal = pal, values = boston_districts@data$journeys, opacity = 1.0)

  return(leaflet_map)
}
