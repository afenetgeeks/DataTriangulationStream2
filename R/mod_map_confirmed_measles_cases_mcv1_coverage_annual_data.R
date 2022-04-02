#' map_confirmed_measles_cases_mcv1_coverage_annual_data UI Function
#'
#' @description A shiny Module. slide 11
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4( textOutput(ns("mvc_title"))),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(leafletOutput(ns("mvcMap"), height=440),type = 6, size = 0.4,hide.ui = F),
          #absolutePanel(top = 160, left = 23,downloadButton("dl1", label = NULL)),
          h6("Quick guide!!"),
          tags$i(style="color:#0e7290;font-size:10px", "The blue bubbles represent clusters of measles cases in a State. The numbers in each bubble are cases in that cluster"),
          br(),
          tags$i(style="color:#0e7290;font-size:10px","Hover over a cluster bubble to see the states it covers. Click on a cluster bubble to zoom in a cluster")
        )

      ))


  )
}

#' map_confirmed_measles_cases_mcv1_coverage_annual_data Server Functions
#' @importFrom leaflet leaflet renderLeaflet colorFactor addProviderTiles setView addPolygons addMarkers labelOptions addLegend markerClusterOptions
#' @importFrom GADMTools gadm_subset
#' @importFrom dplyr left_join
#' @noRd
mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server <- function(id,
                                                                             picker_year_var,
                                                                             picker_month_var,
                                                                             picker_state_var
                                                                            ){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    # Map 11 MVC

    stream2_data<- reactiveValues()

    observe({

      if(sum(picker_state_var() == "Federal Government") == 1){

      stream2_data$dhis2_data <- dplyr::tbl(stream2_pool, "measles_coverage_s11_states") %>%
        filter(Year %in% !!picker_year_var())%>%dplyr::collect() %>%
        dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                      State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }
    else{
      stream2_data$dhis2_data <- dplyr::tbl(stream2_pool, "measles_coverage_s11_states") %>%
      filter(Year %in% !!picker_year_var() &
               State %in% !!picker_state_var())%>%dplyr::collect() %>%
      dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                    State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))
    }





    if(sum(picker_state_var() == "Federal Government") == 1){
      stream2_data$sormas_mvc <- dplyr::tbl(stream2_pool, "sormas_measles_geocodes") %>%
        filter(Year %in% !!picker_year_var()) %>% dplyr::collect()%>%
        dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                      State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }else{

      stream2_data$sormas_mvc <- dplyr::tbl(stream2_pool, "sormas_measles_geocodes") %>%
        filter(Year == !!picker_year_var()&
                 State %in% !!picker_state_var()) %>% dplyr::collect()%>%
        dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                      State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }
    })


    mvc_text <- reactive({
      paste("Chart 10: Confirmed Measles cases, MCV1 coverage (Annual data) ",picker_year_var())
    })

    output$mvc_title <- renderText({
      mvc_text()
    })



    mvc_map_leaflet <-  reactive({

      pal_mvc <- colorFactor(c('red','yellow','green','#424242'),
                             levels = c("0 - 50%","50 - 85%","85 - 100%", "> 100%"),
                             na.color = 'red')

      if(sum(picker_state_var() == "Federal Government") == 1){

        states_gadm_sp_data$spdf@data <- states_gadm_sp_data$spdf@data %>%
          left_join(as.data.frame(stream2_data$dhis2_data), by = c("NAME_1" = "State"))

        mvc_map <-  leaflet() %>%
          addProviderTiles("Stamen.Toner") %>%
          setView(lat =  9.077751,lng = 8.6774567, zoom = 6)  %>%
          addPolygons(data = states_gadm_sp_data$spdf,
                      fillColor = ~pal_mvc(states_gadm_sp_data$spdf@data$`Coverage %`),
                      stroke = TRUE,
                      color = "black",
                      weight = 2.5,
                      fillOpacity = 0.5,
                      fill = T,
                      label = ~paste0(
                        "<h6>", states_gadm_sp_data$spdf@data$NAME_1, "</h6>"
                      )%>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = ,
                        textsize = "13px",
                        direction = "auto", noHide = T,textOnly = T
                      )) %>%
          addLegend(colors = make_shapes(colors = colors(), sizes = sizes() , borders = borders() , shapes = shapes()),
                    labels = make_labels(sizes = sizes(), labels = labels()),
                    opacity =  0.6, title = "Coverage %", position = "bottomright")

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
          addProviderTiles("Stamen.Toner") %>%
          # setView(lat =  states_gadm_sp_data_state$spdf@data$Lat,
          #         lng = states_gadm_sp_data_state$spdf@data$Long, zoom = 6)  %>%
          addPolygons(data = states_gadm_sp_data_state$spdf,
                      fillColor = ~pal_mvc(states_gadm_sp_data_state$spdf@data$`Coverage %`),
                      stroke = TRUE,
                      color = "black",
                      weight = 2.5,
                      fillOpacity = 0.5,
                      fill = T,
                      label = ~paste0(
                        "<h6>", states_gadm_sp_data_state$spdf@data$NAME_1, "</h6>"
                      )%>%
                        lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                        style = font_plot(),
                        #textsize = "13px",
                        direction = "auto", noHide = T,textOnly = T
                      ))%>%
          leaflet::addMarkers(data = stream2_data$sormas_mvc,
                     # layerId = paste0("marker", 1:length(sormas_mvc$Lat)),
                     # clusterId = "clusterIdm",
                     #clusterId = paste0("marker", 1:length(sormas_mvc$Lat)),
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
                    opacity =  0.6, title = "Coverage %", position = "bottomright")

      }

      mvc_map

    })


    output$mvcMap = renderLeaflet({mvc_map_leaflet()})

    output$download_chart_data <- downloadHandler(
      filename = 'mvc_csvs.zip',
      content = function(fname) {

        write.csv(stream2_data$dhis2_data, file = "mvc_admin.csv", sep =",")
        write.csv(stream2_data$sormas_mvc, file = "mvc_sormas.csv", sep =",")

        zip(zipfile=fname, files=c("mvc_admin.csv","mvc_sormas.csv"))
      },
      contentType = "application/zip"
    )


  })
}

## To be copied in the UI
# mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")

## To be copied in the server
# mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")
