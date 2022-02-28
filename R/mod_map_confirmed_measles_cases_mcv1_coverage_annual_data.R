#' map_confirmed_measles_cases_mcv1_coverage_annual_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
                      f7DownloadButton(ns("download_ch11Data"),label = NULL),
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
#'
#' @noRd
mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server <- function(id, picker_year_var,picker_month_var,picker_state_var){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    stream2_data<- reactiveValues()



    observe({

      if(picker_state_var()  == "Federal Government"){

      stream2_data$slide11_data <- mvc_gadm_data %>%
        filter(Year == picker_year_var())}
    else{stream2_data$slide11_data <- mvc_gadm_data %>%
      filter(Year == picker_year_var() &
               State == picker_state_var())}

    if(picker_state_var()  == "Federal Government"){
      stream2_data$sormas_mvc <- sm_plus_lga_latlon_cleaned %>%
        filter(Year == picker_year_var())}

    else{stream2_data$sormas_mvc <- sm_plus_lga_latlon_cleaned %>%
      filter(Year == picker_year_var()&
               `Responsible state` == picker_state_var())}
    })


    # slide 11
    # if(reactive({picker_state_var()})  == "Federal Government"){
    #
    #   slide11_data <- reactive({mvc_gadm_data %>%
    #     filter(Year == picker_year_var())})
    #
    #   }
    # else{slide11_data <- reactive({mvc_gadm_data %>%
    #   filter(Year == picker_year_var() &
    #            State == picker_state_var())})
    # }
    #
    # if(reactive({picker_state_var()})  == "Federal Government"){
    #   sormas_mvc <- reactive({sm_plus_lga_latlon_cleaned %>%
    #     filter(Year == picker_year_var())})
    #     }
    #
    # else{reactive({sormas_mvc}) <- reactive({sm_plus_lga_latlon_cleaned %>%
    #   filter(Year == picker_year_var()&
    #            `Responsible state` == picker_state_var())})
    #   }


    mvc_text <- reactive({
      paste("Chart 11: Confirmed Measles cases, MCV1 coverage (Annual data) ",picker_year_var())
    })

    output$mvc_title <- renderText({
      mvc_text()
    })



    mvc_map_leaflet <-  reactive({

      pal_mvc <- colorFactor(c('red','yellow','green','#424242'),
                             levels = c("0 - 50%","50 - 85%","85 - 100%", "> 100%"),
                             na.color = 'red')

      if(picker_state_var()  == "Federal Government"){

        gadm_data_mvc$spdf@data <- gadm_data_mvc$spdf@data %>%
          left_join(stream2_data$slide11_data, by = c("NAME_1" = "State"))

        mvc_map <-  leaflet() %>%
          addProviderTiles("Stamen.Toner") %>%
          setView(lat =  9.077751,lng = 8.6774567, zoom = 6)  %>%
          addPolygons(data = gadm_data_mvc$spdf,
                      fillColor = ~pal_mvc(gadm_data_mvc$spdf@data$`Coverage %`),
                      stroke = TRUE,
                      color = "black",
                      weight = 2.5,
                      fillOpacity = 0.5,
                      fill = T,
                      label = ~paste0(
                        "<h6>", gadm_data_mvc$spdf@data$NAME_1, "</h6>"
                      )%>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", color = "black"),
                        textsize = "13px",
                        direction = "auto", noHide = T,textOnly = T
                      )) %>%
          addLegend(colors = legend_colors, labels = legend_labels,
                    opacity =  0.6, title = "Coverage %", position = "bottomright")

        mvc_map <- add_state_clusters(leaflet_map =  mvc_map,states = responsible_states_measles, data =  stream2_data$sormas_mvc)

      }
      else{
        gadm_data_mvc_state <- gadm_subset(gadm_data_mvc,
                                           level = 1,
                                           regions = picker_state_var())

        gadm_data_mvc_state$spdf@data <- gadm_data_mvc_state$spdf@data %>%
          left_join(stream2_data$slide11_data, by = c("NAME_1" = "State"))

        mvc_map <-  leaflet() %>%
          addProviderTiles("Stamen.Toner") %>%
          # setView(lat =  gadm_data_mvc_state$spdf@data$Lat,
          #         lng = gadm_data_mvc_state$spdf@data$Long, zoom = 6)  %>%
          addPolygons(data = gadm_data_mvc_state$spdf,
                      fillColor = ~pal_mvc(gadm_data_mvc_state$spdf@data$`Coverage %`),
                      stroke = TRUE,
                      color = "black",
                      weight = 2.5,
                      fillOpacity = 0.5,
                      fill = T,
                      label = ~paste0(
                        "<h6>", gadm_data_mvc_state$spdf@data$NAME_1, "</h6>"
                      )%>%
                        lapply(htmltools::HTML),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", color = "black"),
                        textsize = "13px",
                        direction = "auto", noHide = T,textOnly = T
                      ))%>%
          addMarkers(data = stream2_data$sormas_mvc,
                     # layerId = paste0("marker", 1:length(sormas_mvc$Lat)),
                     # clusterId = "clusterIdm",
                     #clusterId = paste0("marker", 1:length(sormas_mvc$Lat)),
                     lat = ~Lat,
                     lng = ~Long,
                     clusterOptions = markerClusterOptions(maxClusterRadius = 40,
                                                           singleMarkerMode = TRUE,
                                                           showCoverageOnHover = FALSE,
                                                           iconCreateFunction =
                                                             JS("function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(78, 224, 237, 0.7)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }"))) %>%
          addLegend(colors = legend_colors, labels = legend_labels,
                    opacity =  0.6, title = "Coverage %", position = "bottomright")

      }

      mvc_map

    })


    output$mvcMap = renderLeaflet({mvc_map_leaflet()})

    output$download_ch11Data <- downloadHandler(
      filename = 'mvc_csvs.zip',
      content = function(fname) {

        write.csv(stream2_data$slide11_data, file = "mvc_admin.csv", sep =",")
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
