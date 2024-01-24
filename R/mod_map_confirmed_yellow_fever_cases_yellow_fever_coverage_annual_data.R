#' @rdname mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui
#'
#'
mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-12 col-12-t yf-col map_col",

        span(class = "info-icon-container",
             tags$a(class = "info-icon-link", href="#",
                    img(class = "info-icon", src = "www/info_icon.svg", alt="info-icon"),
                    span(class="info-tooltiptext",
                         p(style="color:#ffffff;font-size:10px;algin:left;", "The blue bubbles represent clusters of yellow fever cases in a State. The numbers in each bubble are cases in that cluster"),
                         p(style="color:#ffffff;font-size:10px;algin:left;","Click on a cluster bubble to zoom in a cluster")))),

        div(class ="column-icon-div yf-column-icon-div",
            img(class = "column-icon", src = "www/fully-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title column-title-map'>Chart 5: Confirmed yellow fever cases and coverage</h6>"),


        div(class = "map_charts_inputs",

            pickerInput(inputId = ns("picker_year"), label =  NULL,
                        choices = years_vector_util(), multiple = F, selected = "2023",
                        options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

            pickerInput(inputId = ns("picker_month"), label =  NULL,
                        choices = c("Year Data", months_vector_util()), multiple = F, selected = "Year Data",
                        options = list(title = "Months",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

            pickerInput(ns("picker_state"),label = NULL,
                        choices = c(national_util(),sort(states_vector_util())), multiple = T,selected = national_util(),
                        options = list(title = "State",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

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
        withSpinner(leafletOutput(ns("yfcMap"), height=440),type = 6, size = 0.4,hide.ui = F)
    )

 )
}

#' @rdname mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server
#'
mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_server <- function(id ){

  moduleServer( id, function(input, output, session){
    ns <- session$ns


    picker_state_var <- reactive({input$picker_state})
    picker_year_var <- reactive({ input$picker_year})
    picker_month_var <- reactive({input$picker_month})

    # slide 12
    stream2_data <- reactiveValues()

    observe({

      req(picker_state_var(), cancelOutput = T)


      if(sum(picker_state_var() == "Federal Government") == 1){

        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$dhis2_data <- dplyr::tbl(connection, "yf_coverage_map") %>%
            filter(Year %in% !!picker_year_var() & Months %in% "Ann" & LGA  %in% "State level data") %>% dplyr::collect() %>%
            mutate("Coverage %" = case_when(
              Coverage < 50 ~ "0 - 49%",
              Coverage >= 50 & Coverage < 80 ~ "50 - 79%",
              Coverage >= 80 & Coverage <= 100 ~ "80 - 100%",
              Coverage > 100 ~ "> 100%",
              TRUE ~ "Others"
            )) %>%
            mutate(`Coverage %` = factor(`Coverage %`, levels = c("0 - 49%", "50 - 79%", "80 - 100%", "> 100%"))) %>%
            dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor))

        }else{

          stream2_data$dhis2_data <- dplyr::tbl(connection, "yf_coverage_map") %>%
            filter(Year %in% !!picker_year_var() & Months  %in% !!picker_month_var() & LGA  %in% !!"State level data") %>% dplyr::collect() %>%
            mutate("Coverage %" = case_when(
              Coverage < 50 ~ "0 - 49%",
              Coverage >= 50 & Coverage < 80 ~ "50 - 79%",
              Coverage >= 80 & Coverage <= 100 ~ "80 - 100%",
              Coverage > 100 ~ "> 100%",
              TRUE ~ "Others"
            )) %>%
            mutate(`Coverage %` = factor(`Coverage %`, levels = c("0 - 49%", "50 - 79%", "80 - 100%", "> 100%"))) %>%
            dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor))

        }

      }else{

        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$dhis2_data <- dplyr::tbl(connection, "yf_coverage_map") %>%
            filter(Year %in% !!picker_year_var() &
                     State %in% !!picker_state_var() & Months %in% "Ann" & LGA %in% "State level data")%>%dplyr::collect() %>%
            mutate("Coverage %" = case_when(
              Coverage < 50 ~ "0 - 49%",
              Coverage >= 50 & Coverage < 80 ~ "50 - 79%",
              Coverage >= 80 & Coverage <= 100 ~ "80 - 100%",
              Coverage > 100 ~ "> 100%",
              TRUE ~ "Others"
            )) %>%
            mutate(`Coverage %` = factor(`Coverage %`, levels = c("0 - 49%", "50 - 79%", "80 - 100%", "> 100%"))) %>%
            dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor))

        }else{

          stream2_data$dhis2_data <- dplyr::tbl(connection, "yf_coverage_map") %>%
            filter(Year %in% !!picker_year_var() &
                     State %in% !!picker_state_var() & Months %in% !!picker_month_var() & LGA %in% "State level data")%>%dplyr::collect() %>%
            mutate("Coverage %" = case_when(
              Coverage < 50 ~ "0 - 49%",
              Coverage >= 50 & Coverage < 80 ~ "50 - 79%",
              Coverage >= 80 & Coverage <= 100 ~ "80 - 100%",
              Coverage > 100 ~ "> 100%",
              TRUE ~ "Others"
            )) %>%
            mutate(`Coverage %` = factor(`Coverage %`, levels = c("0 - 49%", "50 - 79%", "80 - 100%", "> 100%"))) %>%
            dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor))

        }



      }



      ################

      if(sum(picker_state_var() == "Federal Government") == 1){

        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$sormas_yfc <- dplyr::tbl(connection, "yf_cases_map") %>%
            filter(Year %in% !!picker_year_var()) %>% dplyr::collect()%>%
            dplyr::mutate(dplyr::across(.col = c(Year,State, Months, LGA), as.factor))

        }else{

          stream2_data$sormas_yfc <- dplyr::tbl(connection, "yf_cases_map") %>%
            filter(Year %in% !!picker_year_var() & Months %in% !!picker_month_var()) %>%
            dplyr::collect()%>%
            dplyr::mutate(dplyr::across(.col = c(Year,State, Months, LGA), as.factor))

        }

      }else{


        if(sum(picker_month_var() == "Year Data") == 1){

          stream2_data$sormas_yfc <- dplyr::tbl(connection, "yf_cases_map") %>%
            filter(Year == !!picker_year_var()&
                     State %in% !!picker_state_var() ) %>% dplyr::collect()%>%
            dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor))

        }else{

          stream2_data$sormas_yfc <- dplyr::tbl(connection, "yf_cases_map") %>%
            filter(Year == !!picker_year_var()&
                     State %in% !!picker_state_var()  & Months %in% !!picker_month_var()) %>% dplyr::collect()%>%
            dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor))

        }



      }


    })



    yfc_map_leaflef <-  reactive({

      req(picker_state_var(), cancelOutput = T)

      pal_yf <- colorFactor(c('red','yellow','green','#424242'),
                            levels = c("0 - 49%", "50 - 79%", "80 - 100%", "> 100%"),
                            na.color = 'red')

      if(sum(picker_state_var() == "Federal Government") == 1){
        states_gadm_sp_data <- states_gadm_sp_data %>%
          left_join(as.data.frame(stream2_data$dhis2_data),
                    by = c("NAME_1" = "State"))

        yfc_map <-  leaflet() %>%
          addProviderTiles("TomTom.Basic") %>%
          setView(lat =  9.077751,lng = 8.6774567, zoom = 6) %>%
          addPolygons( data = states_gadm_sp_data,
                       fillColor = ~pal_yf(states_gadm_sp_data$`Coverage %`),
                       stroke = TRUE,
                       color = "black",
                       weight = 2.5,
                       fillOpacity = 0.5,
                       fill = T,
                       label = ~paste0("<span>", states_gadm_sp_data$NAME_1,"</span>")%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         textsize = "10px",
                         direction = "auto", noHide = T,textOnly = T
                       )
          ) %>%
          addLegend(colors = make_shapes(colors = colors(), sizes = sizes() , borders = borders() , shapes = shapes()),
                    labels = make_labels(sizes = sizes(), labels = labels()),
                    opacity =  0.6, title = "Coverage %", position = "bottomright")%>%
          addResetMapButton()

        yfc_map  <- add_state_clusters(leaflet_map =   yfc_map ,
                                       states = states_vector_util(),
                                       data =  stream2_data$sormas_yfc)

      }
      else{
        states_gadm_sp_data_state <- states_gadm_sp_data |>
          filter(NAME_1 == picker_state_var())

         states_gadm_sp_data_state <-  states_gadm_sp_data_state %>%
          left_join(as.data.frame(stream2_data$dhis2_data),
                    by = c("NAME_1" = "State"))

        yfc_map <-  leaflet() %>%
          addProviderTiles("TomTom.Basic") %>%
          # setView(lat =   states_gadm_sp_data_state$Lat,
          #         lng =  states_gadm_sp_data_state$Long, zoom = 6) %>%
          addPolygons( data = states_gadm_sp_data_state,
                       fillColor = ~pal_yf( states_gadm_sp_data_state$`Coverage %`),
                       stroke = TRUE,
                       color = "black",
                       weight = 2.5,
                       fillOpacity = 0.5,
                       fill = T,
                       label = ~paste0("<span>",  states_gadm_sp_data_state$NAME_1,"</span>"
                       )%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         textsize = "10px",
                         direction = "auto", noHide = T,textOnly = T
                       )) %>%
          leaflet::addMarkers(data = stream2_data$sormas_yfc,
                              lat = ~Lat, lng = ~Long,

                              clusterOptions = markerClusterOptions(maxClusterRadius = 40,
                                                                    showCoverageOnHover = FALSE,
                                                                    singleMarkerMode = TRUE,
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

      yfc_map

    })



    output$yfcMap = renderLeaflet({yfc_map_leaflef()})



    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 5- Yellow Fever", picker_state_var(), picker_year_var() , picker_month_var() ,".zip")
      },
      content = function(fname) {

        write.csv(stream2_data$dhis2_data, file = "yellow fever coverage.csv", sep =",")
        write.csv(stream2_data$sormas_yfc, file = "sormas yellow fever cases.csv", sep =",")

        zip(zipfile=fname, files=c("yellow fever coverage.csv","sormas yellow fever cases.csv"))
      },
      contentType = "application/zip"
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 5- Yellow Fever", picker_state_var(), picker_year_var() , picker_month_var() ,".png")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        saveWidget(yfc_map_leaflef(), "temp.html", selfcontained = FALSE)
        webshot("temp.html", file = file, cliprect = "viewport")

      }
    )

  })
}

## To be copied in the UI
# mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")

## To be copied in the server
# mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_server("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")
