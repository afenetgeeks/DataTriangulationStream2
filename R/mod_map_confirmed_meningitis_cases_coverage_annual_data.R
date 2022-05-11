#' map_confirmed_meningitis_cases_coverage_annual_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_confirmed_meningitis_cases_coverage_annual_data_ui <- function(id){
  ns <- NS(id)
  tagList(


    div(class = "col-6 col-6-t measles-col map_col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/partially-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        # h6("Chart 10: Confirmed meningitis cases, meningitis 1 coverage (Annual data)", class = "column-title"),
        HTML("<h6 class = 'column-title'>Chart 10: Confirmed <span class = 'measles-span'>Meningitis</span> cases, <span class = 'measles-span'>Meningitis</span> coverage (Annual data)</h6>"),


        div(class = "map_charts_inputs",

            pickerInput(inputId = ns("picker_year"), label =  NULL,
                        choices = years_vector_util(), multiple = F, selected = "2021",
                        options = list(title = "Years",`actions-box` = TRUE,size = 10,`selected-text-format` = "count > 2")),

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

        withSpinner(leafletOutput(ns("mvcMap"), height=440),type = 6, size = 0.4,hide.ui = F),
        p("Quick guide!!"),
        tags$i(style="color:#0e7290;font-size:10px", "The blue bubbles represent clusters of meningitis cases in a State. The numbers in each bubble are cases in that cluster"),
        br(),
        tags$i(style="color:#0e7290;font-size:10px","Click on a cluster bubble to zoom in a cluster")


    )
  )
}

#' map_confirmed_meningitis_cases_coverage_annual_data Server Functions
#'
#' @noRd
mod_map_confirmed_meningitis_cases_coverage_annual_data_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    # Map 11 MVC

    picker_state_var <- reactive({input$picker_state})
    picker_year_var <- reactive({ input$picker_year})


    stream2_data<- reactiveValues()

    observe({

      if(sum(picker_state_var() == "Federal Government") == 1){

        stream2_data$dhis2_data <- dplyr::tbl(stream2_pool, "meningitis_covarage_annaully_states") %>%
          filter(Year %in% !!picker_year_var())%>%dplyr::collect() %>%
          dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }
      else{
        stream2_data$dhis2_data <- dplyr::tbl(stream2_pool, "meningitis_covarage_annaully_states") %>%
          filter(Year %in% !!picker_year_var() &
                   State %in% !!picker_state_var())%>%dplyr::collect() %>%
          dplyr::mutate(dplyr::across(.col = c(Year,State,`Coverage %`), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))
      }





      if(sum(picker_state_var() == "Federal Government") == 1){
        stream2_data$sormas_mvc <- dplyr::tbl(stream2_pool, "meningitis_plus_lga_latlon_cleaned_final") %>%
          filter(Year %in% !!picker_year_var()) %>% dplyr::collect()%>%
          dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }else{

        stream2_data$sormas_mvc <- dplyr::tbl(stream2_pool, "meningitis_plus_lga_latlon_cleaned_final") %>%
          filter(Year == !!picker_year_var()&
                   State %in% !!picker_state_var()) %>% dplyr::collect()%>%
          dplyr::mutate(dplyr::across(.col = c(Year,State, LGA), as.factor),
                        State = str_replace(State,pattern = "Federal Capital Territory",replacement = "Fct"))

      }
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
        paste0("Chart 10-", picker_state_var(), picker_year_var() ,".zip")
      },
      content = function(fname) {

        write.csv(stream2_data$dhis2_data, file = "meningitis coverage.csv", sep =",")
        write.csv(stream2_data$sormas_mvc, file = "sormas meningitis cases.csv", sep =",")

        zip(zipfile=fname, files=c("meningitis coverage.csv","sormas meningitis cases.csv"))
      },
      contentType = "application/zip"
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 10-", picker_state_var(), picker_year_var() ,".png")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        saveWidget(mvc_map_leaflet(), "temp.html", selfcontained = FALSE)
        webshot("temp.html", file = file, cliprect = "viewport")
        #export(indicator_plot(), file=file)
      }
    )


    # output$download_chart_data <- downloadHandler(
    #   filename = 'mvc_csvs.zip',
    #   content = function(fname) {
    #
    #     write.csv(stream2_data$dhis2_data, file = "mvc_admin.csv", sep =",")
    #     write.csv(stream2_data$sormas_mvc, file = "mvc_sormas.csv", sep =",")
    #
    #     zip(zipfile=fname, files=c("mvc_admin.csv","mvc_sormas.csv"))
    #   },
    #   contentType = "application/zip"
    # )


  })
}

## To be copied in the UI
# mod_map_confirmed_meningitis_cases_coverage_annual_data_ui("map_confirmed_meningitis_cases_coverage_annual_data_1")

## To be copied in the server
# mod_map_confirmed_meningitis_cases_coverage_annual_data_server("map_confirmed_meningitis_cases_coverage_annual_data_1")
