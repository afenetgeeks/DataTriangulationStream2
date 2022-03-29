
#'@importFrom RMariaDB MariaDB
#'@importFrom dplyr collect tbl mutate arrange filter across
#'@importFrom readr read_csv
#'@importFrom GADMTools gadm_sp_loadCountries
#'@importFrom stringr str_c
#'@importFrom magrittr %>%
#'@import dplyr
#'@importFrom tibble tibble
#'
global <- quote({

  ##"sans-serif"
  font2 <- list(
    family = "Trebuchet MS, Helvetica, sans-serif",
    color = "white",
    size = 12)

  font <- list(
    family = "Trebuchet MS, Helvetica, sans-serif",
    color = "black",
    size = 12)

  font_axis_title <- list(
    family = "Trebuchet MS, Helvetica, sans-serif",
    color = "black",
    size = 15)

  font_yaxis_title<- list(
    family = "Trebuchet MS, Helvetica, sans-serif",
    size = 15,
    color = "black")

  #hrbrthemes::import_roboto_condensed()



  dw <- config::get(file = "./inst/app/www/config.yml", "development_stream2")

  stream2_pool <- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    host     = dw$host,
    username = dw$user,
    password = dw$password,
    port     = dw$port,
    dbname   = dw$database,
    minSize = 3,
    maxSize = Inf,    # this could have been omitted since it's the default
    idleTimeout = 3600000  # 1 hour)
  )


  # onStop(function() {
  #   poolClose(stream2_pool)
  # })


  states <- readr::read_csv("./inst/app/www/states_edited.csv")

  national <- states$state_name[38]

  states_vec <- states$state_name[1:37]


# slide 1
#slide1_data <- read_rds("www/data/rds/slide1_data.rds")
slide1_data <-  dplyr::tbl(stream2_pool, "slide1_data") %>%
  dplyr::collect() %>%
  dplyr::mutate(dplyr::across(.col = c(Year,State), as.factor))

# slide 2
#s2_combined <- read_rds("www/data/rds/s2_combined.rds")
s2_combined <-  dplyr::tbl(stream2_pool, "s2_combined")%>%dplyr::collect()%>%
  dplyr::mutate(dplyr::across(.col = c(Year,State ), as.factor))

# slide 3
#s3_combined <- read_rds("www/data/rds/s3_combined.rds")
s3_combined <-  dplyr::tbl(stream2_pool, "s3_combined")%>%dplyr::collect() %>%
  dplyr::mutate(Months = lubridate::month(as.Date(stringr::str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
         dplyr::across(.col = c(Year,State ), as.factor)) %>%
  tibble::tibble() %>% dplyr::arrange(Months)

# slide 4
# mvc_by_age_group <-
#   read_rds("www/data/rds/s4_combined.rds") %>%
#   dplyr::mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35"),
#                               levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")))
mvc_by_age_group <-  dplyr::tbl(stream2_pool, "mvc_by_age_group")%>%dplyr::collect()%>%
  dplyr::mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35"),
                              levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")),
         dplyr::across(.col = c(Year,State, Months ), as.factor))

# slide 5
# yf_by_age_group <-
#   read_rds("www/data/rds/s5_combined.rds") %>%
#   dplyr::mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14",
#                                                       "15-19", "20-24", "25-29", "30-34", ">=35"),
#                               levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")))
yf_by_age_group <-  dplyr::tbl(stream2_pool, "yf_by_age_group")%>%dplyr::collect() %>%
  dplyr::mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14",
                                                      "15-19", "20-24", "25-29", "30-34", ">=35"),
                              levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")),
         dplyr::across(.col = c(Year,State, Months ), as.factor))


# slide 6
# s6_combined <- read_rds("www/data/rds/s6_combined.rds")
s6_combined <-  dplyr::tbl(stream2_pool, "s6_combined")%>%dplyr::collect() %>%
  dplyr::mutate(`Measles coverage` = `Measles coverage`*100, Months = lubridate::month(as.Date(stringr::str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),label = T),
         dplyr::across(.col = c(Year,State ), as.factor)) %>%
  tibble::tibble() %>% dplyr::arrange(Months)

# slide 7
# s7_combined <- read_rds("www/data/rds/s7_combined.rds")
s7_combined <-  dplyr::tbl(stream2_pool, "s7_combined")%>%dplyr::collect() %>%
  dplyr::mutate(`Yellow Fever Coverage` = `Yellow Fever Coverage`*100, Months = lubridate::month(as.Date(stringr::str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
         dplyr::across(.col = c(Year,State ), as.factor)) %>%
  tibble::tibble() %>% dplyr::arrange(Months)


# slide 8
# s8_combined <- read_rds("www/data/rds/s8_combined.rds")
s8_combined <-  dplyr::tbl(stream2_pool, "s8_combined")%>%dplyr::collect()%>%
  dplyr::mutate(Months = lubridate::month(as.Date(stringr::str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
         dplyr::across(.col = c(Year,State ), as.factor)) %>% tibble::tibble() %>%
  dplyr::arrange(Months)

# slide 9
# s9_combined <- read_rds("www/data/rds/s9_combined.rds")
s9_combined <-  dplyr::tbl(stream2_pool, "s9_combined")%>%dplyr::collect()%>%
  dplyr::mutate(dplyr::across(.col = c(Year,State, Months ), as.factor))

# slide 10
# s10_combined <- read_rds("www/data/rds/s10_combined.rds")
s10_combined <-  dplyr::tbl(stream2_pool, "s10_combined")%>%dplyr::collect()%>%
  dplyr::mutate(dplyr::across(.col = c(Year,State, Months), as.factor))

  # Map 11 MVC
  mvc_gadm_data <-  dplyr::tbl(stream2_pool, "measles_coverage_s11_states")%>%dplyr::collect() %>%
           dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor))

  # mvc_gadm_data <-  dplyr::tbl(stream2_pool, "mvc_gadm_data")%>%dplyr::collect()
  #mvc_gadm_data <- read_rds("www/data/rds/mvc_gadm_data.rds")

  #sormas_measles_cases <-  dplyr::tbl(stream2_pool, "sormas_measles_cases")%>%dplyr::collect()%>%

  sm_plus_lga_latlon_cleaned <-  dplyr::tbl(stream2_pool, "sormas_measles_geocodes")%>%dplyr::collect() %>%
    dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor))
  #sormas_measles_cases <- read_rds("www/data/rds/sormas_measles_cases.rds")

  #gadm_data_mvc <- read_rds("www/gadm36_NGA_1_sp.rds")

  # Map 12 YF
  yfc_gadm_data <-  dplyr::tbl(stream2_pool, "yf_coverage_s12_states")%>% dplyr::collect()%>%
    dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor))


  #yfc_gadm_data <-  dplyr::tbl(stream2_pool, "yfc_gadm_data")%>%dplyr::collect()
  #yfc_gadm_data <- read_rds("./flexible-version/www/data/rds/yfc_gadm_data.rds")


  #sormas_yf_cases <- read_rds("www/data/rds/sormas_yf_cases.rds")
  #sormas_yf_cases <-  dplyr::tbl(stream2_pool, "sormas_yf_cases")%>%dplyr::collect()%>%
  syf_plus_lga_latlon_cleaned <-  dplyr::tbl(stream2_pool, "sormas_yf_geocodes")%>%
    dplyr::collect()%>%dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor))

  #gadm_data_yfc <- read_rds("www/gadm36_NGA_1_sp.rds")


  colors <- c('red','yellow','#61ed61', '#424242', "#4ee0ed")
  labels <- c("0 - 50%", "50 - 85%", "85 - 100%", "> 100%", "Cases from Sormas")
  sizes <- c(20,20,20,20,20)
  shapes <- c("square", "square","square", "square", "square" )
  borders <- c('red','yellow','#61ed61', '#424242',"#4ee0ed")

  # -----------------------------------------------------------------
  gadm_data_mvc <- GADMTools::gadm_sp_loadCountries(c("NGA"), level=1, basefile = "./inst/app/www/")
  gadm_data_yfc <- GADMTools::gadm_sp_loadCountries(c("NGA"), level=1, basefile = "./inst/app/www/")


  ## Creating  add_state_clusters function to create clusters at state level.

  responsible_states_measles <- levels(sm_plus_lga_latlon_cleaned$State)
  responsible_states_yf <- levels(syf_plus_lga_latlon_cleaned$State)


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


  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("square", "50%", shapes)

    shapes <- gsub("square", "0%", shapes)

    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }

  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ",
           sizes, "px;margin-top: 4px;line-height: ",
           sizes, "px;'>", labels, "</div>")
  }

  legend_colors <- make_shapes(colors, sizes, borders, shapes)

  legend_labels <- make_labels(sizes, labels)

  legend_colors_var <- make_shapes(colors, sizes, borders, shapes)

  legend_labels_var <- make_labels(sizes, labels)

  pool::poolClose(stream2_pool)
})


