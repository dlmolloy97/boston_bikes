#' @export
#' This function extracts latest zipfile for user data
extract_zip <- function( ) {
  usethis::edit_r_environ("project")
  #Citation: https://stackoverflow.com/questions/23899525/using-r-to-download-zipped-data-file-extract-and-import-csv
  download.file(Sys.getenv("zip_path"),"data/latest.zip", mode="wb")
  system("unzip -d data/ data/latest.zip -y")
}

enrich_geospatial <- function( ) {
  stations <- read.csv(Sys.getenv("station_path"), header = TRUE, skip = 1)
  trip_data <- read.csv(Sys.getenv("in_path"))
  neighbourhoods <- rgdal::readOGR("data/Boston_Neighborhoods.geojson")
  stations_enriched <- sf::st_as_sf(stations,
                                    coords=c('Longitude', 'Latitude'), crs=sf::st_crs(neighbourhoods))

  neighbourhood_geospatial <- neighbourhoods %>%
    sf::st_as_sf() %>%
    sf::st_join(stations_enriched, join=st_intersects)

  trip_data_enriched <- trip_data %>%
    dplyr::mutate(start.station.id = as.integer(start.station.id), end.station.id = as.integer(end.station.id))%>%
    dplyr::left_join(neighbourhood_geospatial, by = c('start.station.name' = 'Name.y'))%>%
    dplyr::rename(start_neighbourhood = Name.x)%>%
    dplyr::left_join(neighbourhood_geospatial, by = c('end.station.name' = 'Name.y'))%>%
    dplyr::rename(end_neighbourhood = Name.x)%>%
    dplyr::select(tripduration,starttime,stoptime,start.station.name,end.station.name, bikeid, usertype, start_neighbourhood, end_neighbourhood, Deployment.Year.x, Deployment.Year.y)%>%
    dplyr::rename(start_neighbourhood_deployment_year = Deployment.Year.x, end_neighbourhood_deployment_year = Deployment.Year.y)
  return(trip_data_enriched)
}
