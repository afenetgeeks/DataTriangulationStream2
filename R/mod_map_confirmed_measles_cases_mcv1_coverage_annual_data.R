#' Create shiny module UI side for Chart 7 for Measles and chart 5 (Yellow and Meningitis )
#'
#' @description `mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui()`
#' Creates a shiny Module UI of map of confirmed Measles cases, MCV 1 coverage
#'  currently chart 7
#'
#' @inherit mod_confirmed_measles_cases_MCV1_coverage_ui return
#'
mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(

        div(class = "col-12 col-12-t measles-col",

        span(class = "info-icon-container",
             tags$a(class = "info-icon-link", href="#",
                    img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                    span(class="info-tooltiptext",
                         p(style="color:#ffffff;font-size:10px;algin:left;", "The blue bubbles represent clusters of measles cases in a State. The numbers in each bubble are cases in that cluster"),
                         p(style="color:#ffffff;font-size:10px;algin:left;","Click on a cluster bubble to zoom in a cluster")))),

        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/partially-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title column-title-map'>Chart 7: Confirmed Measles cases, MCV 1 coverage </h6>"),


        div(class = "map_charts_inputs",

            pickerInput(inputId = ns("picker_year"), label =  NULL,
                        choices = years_vector_util(), multiple = F, selected = "2022",
                        options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

            pickerInput(inputId = ns("picker_month"), label =  NULL,
                        choices = c("Year Data", months_vector_util()), multiple = F, selected = "Year Data",
                        options = list(title = "Months",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

            pickerInput(ns("picker_state"),label = NULL,
                        choices = c(national_util(),sort(states_vector_util())), multiple = T,selected = national_util(),
                        options = list(title = "State",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2"))

        ),


       HTML(paste0('<a id="', ns("downloadData"), '" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>
                      <i class="fa fa-download" aria-hidden="true"></i>
                      <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>
                     </a>')),

       HTML(paste0('<a id="', ns("downloadChart"), '" class="btn btn-default shiny-download-link download-data-btn download-chart-btn" href="" target="_blank" download>
                     <i class="fa fa-chart-bar"></i>
                      <div class = tooltipdiv>
                          <p class="tooltiptext">
                              Download this Chart
                          </p>
                      </div>
                     </a>')),

        withSpinner(leafletOutput(ns("mvcMap"), height=440),type = 6, size = 0.4,hide.ui = F)
    )
  )
}

#' Create shiny module server side for chart 7 and chart 5 (Yellow and Meningitis )
#'
#' @description `mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server()`
#'  Creates a shiny Module for the server of map of confirmed Measles cases, MCV 1 coverage
#'  currently chart 7
#'
#' @inherit mod_confirmed_measles_cases_MCV1_coverage_server return
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    # Map 11 MVC

    picker_state_var <- reactive({input$picker_state})
    picker_year_var <- reactive({ input$picker_year})
    picker_month_var <- reactive({input$picker_month})

    stream2_data<- reactiveValues()

    observe({

      req(picker_state_var(), cancelOutput = T)

      if(sum(picker_state_var() == "Federal Government") == 1){

        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$dhis2_data <- dplyr::tbl(connection, "measles_coverage_map") %>%
        filter(Year %in% !!picker_year_var() & Months %in% "Ann" & LGA  %in% "State level data") %>% dplyr::collect() %>%
        dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                      State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }else{

        stream2_data$dhis2_data <- dplyr::tbl(connection, "measles_coverage_map") %>%
          filter(Year %in% !!picker_year_var() & Months  %in% !!picker_month_var() & LGA  %in% !!"State level data") %>% dplyr::collect() %>%
          dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }

      }else{

        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$dhis2_data <- dplyr::tbl(connection, "measles_coverage_map") %>%
            filter(Year %in% !!picker_year_var() &
                     State %in% !!picker_state_var() & Months %in% "Ann" & LGA %in% "State level data")%>%dplyr::collect() %>%
            dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                          State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

        }else{

          stream2_data$dhis2_data <- dplyr::tbl(connection, "measles_coverage_map") %>%
            filter(Year %in% !!picker_year_var() &
                     State %in% !!picker_state_var() & Months %in% !!picker_month_var() & LGA %in% "State level data")%>%dplyr::collect() %>%
            dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                          State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))
       }
      }



################

    if(sum(picker_state_var() == "Federal Government") == 1){

      if(sum(picker_month_var() == "Year Data") == 1){

      stream2_data$sormas_mvc <- dplyr::tbl(connection, "measles_cases_map") %>%
        filter(Year %in% !!picker_year_var()) %>% dplyr::collect()%>%
        dplyr::mutate(dplyr::across(.col = c(Year,State, Months, LGA), as.factor),
                      State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }else{

        stream2_data$sormas_mvc <- dplyr::tbl(connection, "measles_cases_map") %>%
          filter(Year %in% !!picker_year_var() & Months %in% !!picker_month_var()) %>%
          dplyr::collect()%>%
          dplyr::mutate(dplyr::across(.col = c(Year,State, Months, LGA), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))
        }

      }else{


        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$sormas_mvc <- dplyr::tbl(connection, "measles_cases_map") %>%
            filter(Year == !!picker_year_var()&
                     State %in% !!picker_state_var() ) %>% dplyr::collect()%>%
            dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                          State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

        }else{

          stream2_data$sormas_mvc <- dplyr::tbl(connection, "measles_cases_map") %>%
            filter(Year == !!picker_year_var()&
                     State %in% !!picker_state_var()  & Months %in% !!picker_month_var()) %>% dplyr::collect()%>%
            dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                          State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

        }

      }
    })


    ###################


    mvc_map_leaflet <-  reactive({

      req(picker_state_var(), cancelOutput = T)

      pal_mvc <- colorFactor(c('red','yellow','green','#424242'),
                             levels = c("0 - 50%","50 - 85%","85 - 100%", "> 100%"),
                             na.color = 'red')

      if(sum(picker_state_var() == "Federal Government") == 1){

        states_gadm_sp_data$spdf@data <- states_gadm_sp_data$spdf@data %>%
          left_join(as.data.frame(stream2_data$dhis2_data), by = c("NAME_1" = "State"))

        mvc_map <-  leaflet() %>%
          addProviderTiles("TomTom.Basic") %>%
          setView(lat =  9.077751,lng = 8.6774567, zoom = 6)  %>%
          addPolygons(data = states_gadm_sp_data$spdf,
                      fillColor = ~pal_mvc(states_gadm_sp_data$spdf@data$`Coverage %`),
                      stroke = TRUE,
                      color = "black",
                      weight = 2.5,
                      fillOpacity = 0.5,
                      fill = T,
                      label = ~paste0(
                        "<span>", states_gadm_sp_data$spdf@data$NAME_1, "</span>"
                      )%>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        textsize = "10px",
                        direction = "auto", noHide = T,textOnly = T
                      )) %>%
          addLegend(colors = make_shapes(colors = colors(), sizes = sizes() , borders = borders() , shapes = shapes()),
                    labels = make_labels(sizes = sizes(), labels = labels()),
                    opacity =  0.6, title = "Coverage %", position = "bottomright") %>%
          addResetMapButton()

        mvc_map <- add_state_clusters(leaflet_map =  mvc_map,
                                      states = str_replace(states_vector_util(),pattern = "Federal Capital Territory",replacement = "Fct"),
                                      data =  stream2_data$sormas_mvc)

      }
      else{
        states_gadm_sp_data_state <- gadm_subset(states_gadm_sp_data,
                                                 level = 1,
                                                 regions = picker_state_var())

        states_gadm_sp_data_state$spdf@data <- states_gadm_sp_data_state$spdf@data %>%
          left_join(as.data.frame(stream2_data$dhis2_data), by = c("NAME_1" = "State"))

        mvc_map <-  leaflet() %>%
          addProviderTiles("TomTom.Basic") %>%
          # setView(lat =  states_gadm_sp_data_state$spdf@data$Lat,
          #         lng = states_gadm_sp_data_state$spdf@data$Long, zoom = 6)  %>%
          addPolygons(data = states_gadm_sp_data_state$spdf,
                      fillColor = ~pal_mvc(states_gadm_sp_data_state$spdf@data$`Coverage %`),
                      stroke = TRUE,
                      color = "black",
                      weight = 2.5,
                      fillOpacity = 0.5,
                      fill = T,
                      label = ~paste0("<span>", states_gadm_sp_data_state$spdf@data$NAME_1,"</span>"
                      )%>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        textsize = "10px",
                        direction = "auto", noHide = T,textOnly = T
                      )) %>%
          leaflet::addMarkers(data = stream2_data$sormas_mvc,

                              lat = ~Lat,
                              lng = ~Long,
                              clusterOptions = markerClusterOptions(maxClusterRadius = 40,
                                                                    singleMarkerMode = TRUE,
                                                                    showCoverageOnHover = FALSE,
                                                                    iconCreateFunction =
                                                                      htmlwidgets::JS("function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(78, 224, 237, 0.7)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
          addLegend(colors = make_shapes(colors = colors(), sizes = sizes() , borders = borders() , shapes = shapes()),
                    labels = make_labels(sizes = sizes(), labels = labels()),
                    opacity =  0.6, title = "Coverage %", position = "bottomright")%>%
          addResetMapButton()

      }

      mvc_map

    })


    output$mvcMap <-  renderLeaflet({mvc_map_leaflet()})





    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 7- Measles",  picker_state_var(), picker_year_var() , picker_month_var() ,".zip")
      },
     content = function(fname) {

        write.csv(stream2_data$dhis2_data, file = "measles coverage.csv", sep =",")

        write.csv(stream2_data$sormas_mvc, file = "sormas measles cases.csv", sep =",")

        zip(zipfile=fname, files=c("measles coverage.csv","sormas measles cases.csv"))
      },
      contentType = "application/zip"
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 7-Measlses",   picker_state_var(), picker_year_var() , picker_month_var() ,".png")
      },
      content = function(file) {

        owd <- setwd(tempdir())
        on.exit(setwd(owd))

        saveWidget(mvc_map_leaflet(), "temp.html", selfcontained = FALSE)
        webshot("temp.html", file = file, cliprect = "viewport")
      }
    )




  })
}

## To be copied in the UI
# mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")

## To be copied in the server
# mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")
