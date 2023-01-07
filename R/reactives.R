#' @export
grouping_neighbourhood_type <- function(direction) {
  return(ifelse(direction=="Inbound","start_neighbourhood", "end_neighbourhood"))
}

selected_neighbourhood_type <- function(direction) {
  return(ifelse(direction=="Inbound","end_neighbourhood", "start_neighbourhood"))
}
