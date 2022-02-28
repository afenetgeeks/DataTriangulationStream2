global <- quote({

  #library
  # Core packages
  library(shinyMobile)
  library(tidyverse)
  library(shiny)
  library(shinyWidgets)
  #library(shinyjs)
  library(shinycssloaders)
  library(htmlwidgets)
  library(hrbrthemes)

  # Interactive Visualizations
  library(plotly)
  library(ggtext)

  library(DBI)
  #library(RMySQL)
  library(RMariaDB)
  library(odbc)
  library(leaflet)
  library(pool)
  #library(mapview)
  library(sp)
  library(leaflet.extras)
  library(GADMTools)


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

  stream2_pool <- dbPool(
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


  states <- read_csv("./inst/app/www/states_edited.csv")

  national <- states$state_name[38]

  states_vec <- states$state_name[1:37]


# slide 1
#slide1_data <- read_rds("www/data/rds/slide1_data.rds")
slide1_data <- tbl(stream2_pool, "slide1_data")%>%collect() %>%
  mutate(across(c(Year,State), as.factor))

# slide 2
#s2_combined <- read_rds("www/data/rds/s2_combined.rds")
s2_combined <- tbl(stream2_pool, "s2_combined")%>%collect()%>%
  mutate(across(c(Year,State ), as.factor))

# slide 3
#s3_combined <- read_rds("www/data/rds/s3_combined.rds")
s3_combined <- tbl(stream2_pool, "s3_combined")%>%collect() %>%
  mutate(Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
         across(c(Year,State ), as.factor)) %>%
  tibble() %>% arrange(Months)

# slide 4
# mvc_by_age_group <-
#   read_rds("www/data/rds/s4_combined.rds") %>%
#   mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35"),
#                               levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")))
mvc_by_age_group <- tbl(stream2_pool, "mvc_by_age_group")%>%collect()%>%
  mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35"),
                              levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")),
         across(c(Year,State ), as.factor))

# slide 5
# yf_by_age_group <-
#   read_rds("www/data/rds/s5_combined.rds") %>%
#   mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14",
#                                                       "15-19", "20-24", "25-29", "30-34", ">=35"),
#                               levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")))
yf_by_age_group <- tbl(stream2_pool, "yf_by_age_group")%>%collect() %>%
  mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14",
                                                      "15-19", "20-24", "25-29", "30-34", ">=35"),
                              levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")),
         across(c(Year,State ), as.factor))


# slide 6
# s6_combined <- read_rds("www/data/rds/s6_combined.rds")
s6_combined <- tbl(stream2_pool, "s6_combined")%>%collect() %>%
  mutate(`Measles coverage` = `Measles coverage`*100, Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),label = T),
         across(c(Year,State ), as.factor)) %>%
  tibble() %>% arrange(Months)

# slide 7
# s7_combined <- read_rds("www/data/rds/s7_combined.rds")
s7_combined <- tbl(stream2_pool, "s7_combined")%>%collect() %>%
  mutate(`Yellow Fever Coverage` = `Yellow Fever Coverage`*100, Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
         across(c(Year,State ), as.factor)) %>%
  tibble() %>% arrange(Months)


# slide 8
# s8_combined <- read_rds("www/data/rds/s8_combined.rds")
s8_combined <- tbl(stream2_pool, "s8_combined")%>%collect()%>%
  mutate(Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
         across(c(Year,State ), as.factor)) %>% tibble() %>%
  arrange(Months)

# slide 9
# s9_combined <- read_rds("www/data/rds/s9_combined.rds")
s9_combined <- tbl(stream2_pool, "s9_combined")%>%collect()%>%
  mutate(across(c(Year,State ), as.factor))

# slide 10
# s10_combined <- read_rds("www/data/rds/s10_combined.rds")
s10_combined <- tbl(stream2_pool, "s10_combined")%>%collect()%>%
  mutate(across(c(Year,State ), as.factor))

  # Map 11 MVC
  mvc_gadm_data <- tbl(stream2_pool, "measles_coverage_s11_states")%>%collect() %>%
    mutate(State = str_replace(string = State, pattern = "Nasarawa",replacement = "Nassarawa")) %>%
    mutate(State = str_replace(string = State, pattern = "FCT",replacement = "Federal Capital Territory"),
           across(c(Year,State,`Coverage %`), as.factor))
  # mvc_gadm_data <- tbl(stream2_pool, "mvc_gadm_data")%>%collect()
  #mvc_gadm_data <- read_rds("www/data/rds/mvc_gadm_data.rds")

  #sormas_measles_cases <- tbl(stream2_pool, "sormas_measles_cases")%>%collect()%>%

  sm_plus_lga_latlon_cleaned <- tbl(stream2_pool, "sormas_measles_geocodes")%>%collect() %>%
    mutate(across(c(Year,`Responsible state`, `Responsible LGA`), as.factor))
  #sormas_measles_cases <- read_rds("www/data/rds/sormas_measles_cases.rds")

  #gadm_data_mvc <- read_rds("www/gadm36_NGA_1_sp.rds")

  # Map 12 YF
  yfc_gadm_data <- tbl(stream2_pool, "yf_coverage_s12_states")%>% collect()%>%
    mutate(State = str_replace(string = State, pattern = "Nasarawa",replacement = "Nassarawa")) %>%
    mutate(State = str_replace(string = State, pattern = "FCT",replacement = "Federal Capital Territory"),
           across(c(Year,State,`Coverage %`), as.factor))


  #yfc_gadm_data <- tbl(stream2_pool, "yfc_gadm_data")%>%collect()
  #yfc_gadm_data <- read_rds("./flexible-version/www/data/rds/yfc_gadm_data.rds")


  #sormas_yf_cases <- read_rds("www/data/rds/sormas_yf_cases.rds")
  #sormas_yf_cases <- tbl(stream2_pool, "sormas_yf_cases")%>%collect()%>%
  syf_plus_lga_latlon_cleaned <- tbl(stream2_pool, "sormas_yf_geocodes")%>%collect()%>%
    mutate(across(c(Year,`Responsible state`, `Responsible LGA`), as.factor))

  #gadm_data_yfc <- read_rds("www/gadm36_NGA_1_sp.rds")


  colors <- c('red','yellow','#61ed61', '#424242', "#4ee0ed")
  labels <- c("0 - 50%", "50 - 85%", "85 - 100%", "> 100%", "Cases from Sormas")
  sizes <- c(20,20,20,20,20)
  shapes <- c("square", "square","square", "square", "square" )
  borders <- c('red','yellow','#61ed61', '#424242',"#4ee0ed")

  # -----------------------------------------------------------------
  gadm_data_mvc <- gadm_sp_loadCountries(c("NGA"), level=1, basefile = "./inst/app/www/")
  gadm_data_yfc <- gadm_sp_loadCountries(c("NGA"), level=1, basefile = "./inst/app/www/")


  ## Creating  add_state_clusters function to create clusters at state level.

  responsible_states_measles <- levels(sm_plus_lga_latlon_cleaned$`Responsible state`)
  responsible_states_yf <- levels(syf_plus_lga_latlon_cleaned$`Responsible state`)


  add_state_clusters <- function(leaflet_map, data, states){

    for(state in states){

      leaflet_map <- leaflet_map %>%
        addMarkers(data = filter(data ,`Responsible state` == state),
                   lat = ~Lat,
                   lng = ~Long,
                   clusterOptions = markerClusterOptions(maxClusterRadius = 200,
                                                         showCoverageOnHover = FALSE,
                                                         singleMarkerMode = TRUE,
                                                         iconCreateFunction =
                                                           JS("function(cluster) {
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

  poolClose(stream2_pool)
})


