#' map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet leafletOutput
mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4(textOutput(ns("yf_title"))),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(leafletOutput(ns("yfcMap"), height=440),type = 6, size = 0.4,hide.ui = F),
          #absolutePanel(top = 160, left = 23 ,downloadButton("dl2", label = NULL)),
          h6("Quick guide!!"),
          tags$i(style="color:#0e7290;font-size:10px","The blue bubbles represent clusters of yellow fever cases in a State. The numbers in each bubble are cases in that cluster"),
          br(),
          tags$i(style="color:#0e7290;font-size:10px","Hover over a cluster bubble to see the states it covers. Click on a cluster bubble to zoom in a cluster")
        )

      )
      )
    )
}

#' map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data Server Functions
#' @importFrom leaflet leaflet renderLeaflet colorFactor addProviderTiles setView addPolygons addMarkers labelOptions addLegend markerClusterOptions
#' @importFrom stringr str_replace
#' @noRd
mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_server <- function(id,
                                                                                          picker_year_var,
                                                                                          picker_month_var,
                                                                                          picker_state_var

                                                                                          ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    yf_text <- reactive({
      paste("Chart 11: Confirmed Yellow Fever cases, Yellow Fever coverage (Annual data),",picker_year_var())
    })

    output$yf_title <- renderText({
      yf_text()
    })

    # slide 12
    stream2_data <- reactiveValues()

    observe({
      if(sum(picker_state_var() == "Federal Government") == 1){

        stream2_data$dhis2_data <- dplyr::tbl(stream2_pool, "yf_coverage_s12_states") %>%
          filter(Year == !!picker_year_var())%>%
          dplyr::collect()%>%
          dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

        }
      else{

        stream2_data$dhis2_data <- dplyr::tbl(stream2_pool, "yf_coverage_s12_states") %>%
          filter(Year == !!picker_year_var() &
                   State %in% !!picker_state_var())%>%
          dplyr::collect()%>%
          dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

        }

      if(sum(picker_state_var() == "Federal Government") == 1){

        stream2_data$sormas_yfc <-  dplyr::tbl(stream2_pool, "sormas_yf_geocodes") %>%
          filter(Year == !!picker_year_var())%>%
          dplyr::collect()%>%
          dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                                           State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

        }
      else{

        stream2_data$sormas_yfc <-  dplyr::tbl(stream2_pool, "sormas_yf_geocodes") %>%
          filter(Year == !!picker_year_var() &
                   State %in% !!picker_state_var())%>%
          dplyr::collect()%>%dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                                           State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))
      }


    })


    yfc_map_leaflef <-  reactive({

      pal_yf <- colorFactor(c('red','yellow','green','#424242'),
                            levels = c("0 - 50%","50 - 85%","85 - 100%", "> 100%"),
                            na.color = 'red')

      if(sum(picker_state_var() == "Federal Government") == 1){
        states_gadm_sp_data$spdf@data <- states_gadm_sp_data$spdf@data %>%
          left_join(as.data.frame(stream2_data$dhis2_data),
                    by = c("NAME_1" = "State"))

        yfc_map <-  leaflet() %>%
          addProviderTiles("Stamen.Toner") %>%
          setView(lat =  9.077751,lng = 8.6774567, zoom = 6) %>%
          addPolygons( data = states_gadm_sp_data$spdf,
                       fillColor = ~pal_yf(states_gadm_sp_data$spdf@data$`Coverage %`),
                       stroke = TRUE,
                       color = "black",
                       weight = 2.5,
                       fillOpacity = 0.5,
                       fill = T,
                       label = ~paste0("<h6>", states_gadm_sp_data$spdf@data$NAME_1,"<h6>")%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = font_plot(),
                         #textsize = "13px",
                         direction = "auto", noHide = T,textOnly = T
                       )
          ) %>%
          addLegend(colors = make_shapes(colors = colors(), sizes = sizes() , borders = borders() , shapes = shapes()),
                    labels = make_labels(sizes = sizes(), labels = labels()),
                    opacity =  0.6, title = "Coverage %", position = "bottomright")

        yfc_map  <- add_state_clusters(leaflet_map =   yfc_map ,
                                       states = str_replace(states_vector_util(),pattern = "Federal Capital Territory",replacement = "Fct"),
                                       data =  stream2_data$sormas_yfc)

      }
      else{
        states_gadm_sp_data_state <- gadm_subset(states_gadm_sp_data,
                                           level = 1,
                                           regions = picker_state_var())

        states_gadm_sp_data_state$spdf@data <- states_gadm_sp_data_state$spdf@data %>%
          left_join(as.data.frame(stream2_data$dhis2_data),
                    by = c("NAME_1" = "State"))

        yfc_map <-  leaflet() %>%
          addProviderTiles("Stamen.Toner") %>%
          # setView(lat =  states_gadm_sp_data_state$spdf@data$Lat,
          #         lng = states_gadm_sp_data_state$spdf@data$Long, zoom = 6) %>%
          addPolygons( data = states_gadm_sp_data_state$spdf,
                       fillColor = ~pal_yf(states_gadm_sp_data_state$spdf@data$`Coverage %`),
                       stroke = TRUE,
                       color = "black",
                       weight = 2.5,
                       fillOpacity = 0.5,
                       fill = T,
                       label = ~paste0("<h6>", states_gadm_sp_data_state$spdf@data$NAME_1,"<h6>"
                                       )%>%
                         lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = font_plot(),
                         #textsize = "13px",
                         direction = "auto", noHide = T,textOnly = T
                       )) %>%
          leaflet::addMarkers(data = stream2_data$sormas_yfc,
                     lat = ~Lat, lng = ~Long,
                     # layerId  = paste0("marker", 1:length(stream2_data$sormas_yfc$Lat)),
                     # clusterId = "clusterIdy",
                     #clusterId = paste0("marker", 1:length(stream2_data$sormas_yfc$Lat)),
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
                    opacity =  0.6, title = "Coverage %", position = "bottomright")

      }

      yfc_map

    })



    output$yfcMap = renderLeaflet({yfc_map_leaflef()})


    # output$dl2 <- downloadHandler(
    #   filename = paste0( Sys.Date()
    #                      , "Confirmed Yellow Fever cases, Yellow Fever coverage by states, 2020"
    #                      , ".png"
    #   )
    #
    #   , content = function(file) {
    #      mapshot( x = yfc_map_leaflef()
    #              , file = file
    #              , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
    #              , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
    #     )
    #   } # end of content() function
    # ) # end of downloadHandler() func)
    #
    output$download_chart_data <- downloadHandler(
      filename = 'yfc_csvs.zip',
      content = function(fname) {

        write.csv(stream2_data$dhis2_data, file = "yfc_admin.csv", sep =",")
        write.csv(stream2_data$sormas_yfc, file = "yfc_sormas.csv", sep =",")

        zip(zipfile=fname, files=c("yfc_admin.csv","yfc_sormas.csv"))
      },
      contentType = "application/zip"
    )






  })
}

## To be copied in the UI
# mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")

## To be copied in the server
# mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_server("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")
