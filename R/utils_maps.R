#' maps
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


colors <- function(){
  c('red','yellow','#61ed61', '#424242', "#4ee0ed")
}

labels <- function(){
  c("0 - 50%", "50 - 85%", "85 - 100%", "> 100%", "Cases from Sormas")
}


sizes <- function(){
  c(20,20,20,20,20)
}


borders <-function(){
  c('red','yellow','#61ed61', '#424242',"#4ee0ed")
}



shapes <- function(){
  c("square", "square","square", "square", "square" )
}

#####

make_shapes <- function(colors, sizes, borders, shapes) {
  shapes <- gsub("square", "50%", shapes)

  shapes <- gsub("square", "0%", shapes)

  paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
}

#

make_labels <- function(sizes, labels) {

  paste0("<div style='display: inline-block;height: ",
         sizes, "px;margin-top: 4px;line-height: ",
         sizes, "px;'>", labels, "</div>")
}



#' Create case clusters at state level.
#'
#' @description
#' `add_state_clusters()` creates clusters for each state level


add_state_clusters <- function(leaflet_map, data, states){

  for(state in states){

    leaflet_map <- leaflet_map %>%
      leaflet:: addMarkers(data = dplyr::filter(data ,State == state),
                           lat = ~Lat,
                           lng = ~Long,
                           clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 200,
                                                                          showCoverageOnHover = FALSE,
                                                                          singleMarkerMode = TRUE,
                                                                          iconCreateFunction =
                                                                            htmlwidgets::JS("function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(78, 224, 237, 0.7)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")))
  }

  return(leaflet_map)

}

