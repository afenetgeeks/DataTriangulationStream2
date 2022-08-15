
#' project_imports
#'
#' @description A utils function
#'
#' @import shiny stringr
#' @importFrom shiny NS tagList
#' @importFrom brochure page
#' @importFrom RMariaDB MariaDB
#' @importFrom pool dbPool
#' @import dbplyr
#' @importFrom dplyr collect tbl mutate arrange filter across group_by summarise ungroup left_join
#' @importFrom readr read_csv
#' @importFrom stringr str_c str_replace
#' @importFrom magrittr %>%
#' @importFrom waiter waiterPreloader spin_loaders
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput renderPlotly plot_ly add_trace layout config add_annotations
#' @importFrom GADMTools gadm_sp_loadCountries gadm_subset
#' @importFrom leaflet leaflet leafletOutput renderLeaflet colorFactor addProviderTiles setView addPolygons addMarkers labelOptions addLegend markerClusterOptions
#' @importFrom leaflet.extras addResetMapButton
#' @importFrom htmlwidgets saveWidget
#' @importFrom webshot webshot
#' @importFrom utils write.csv zip

webshot::install_phantomjs(version = "2.1.1", force = FALSE)

