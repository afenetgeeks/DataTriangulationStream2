#' age_group_of_confirmed_diphtheria_cases_by_vaccination_status UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_ui <- function(id){
  ns <- NS(id)
  tagList(


    div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/age-group-vaccination-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title'> Chart 2: Age group of confirmed diphtheria cases by vaccination status</h6>"),

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

        withSpinner(
          plotlyOutput(ns("plot")),
          type = 6, size = 0.3,hide.ui = F)
    )
  )
}

#' age_group_of_confirmed_diphtheria_cases_by_vaccination_status Server Functions
#'
#' @noRd
mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_server <- function(id,
                                                                                     picker_year_var,
                                                                                     picker_month_var,
                                                                                     picker_state_var,
                                                                                     picker_lga_var ){

  stopifnot(is.reactive(picker_year_var))
  stopifnot(is.reactive(picker_month_var))
  stopifnot(is.reactive(picker_state_var))
  stopifnot(is.reactive(picker_lga_var))

  moduleServer( id, function(input, output, session){

  ns <- session$ns

  chart_data <- reactive({

    dplyr::tbl(connection, "diphtheria_age_group")%>%
      filter(Year %in% !!picker_year_var() &
               State %in% !!picker_state_var() &
               Months %in%  !!picker_month_var() &
               LGA %in% !!picker_lga_var()) %>%group_by(`Age group`) %>%
      summarise(across(c(Vaccinated,Unvaccinated,Unknown), ~ sum(.x, na.rm = TRUE))) %>%  ungroup() %>% dplyr::collect() %>%
      dplyr::mutate(`Age group` = factor(`Age group`, labels = c("< 9","9 - 59" , "60 - 180","> 180"),
                                         levels = c("< 9","9 - 59" , "60 - 180","> 180")))
  })

  # chart_data <-  dplyr::tbl(connection,"diphtheria_age_group") %>%
  #   dplyr::filter(Year %in% !! "2023"&
  #                   Months %in% !!"Aug" &
  #                   State %in% !!"Federal Government" &
  #                   LGA %in% !!"State level data")%>% collect() %>%
  #   group_by(`Age group`) %>%
  #   summarise(across(c(Vaccinated,Unvaccinated,Unknown), ~ sum(.x, na.rm = TRUE))) %>%  ungroup() %>% dplyr::collect() %>%
  #   dplyr::mutate(`Age group` = factor(`Age group`, labels = c("< 9","9 - 59" , "60 - 180","> 180"),
  #                                      levels = c("< 9","9 - 59" , "60 - 180","> 180")))


  indicator_plot <- reactive({

    plot <- plot_ly(chart_data(),
                    x = ~`Age group`,
                    y = ~Unknown,
                    hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                          '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),
                    type = "bar",
                    color =  I("#004e64"),
                    name = "Unknown") %>%
      add_trace(y = ~Unvaccinated,
                color = I("#00a5cf"),
                hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                      '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),

                name = "Unvaccinated") %>%
      add_trace(y = ~Vaccinated,
                hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                      '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),
                color = I("#edb952"),
                name = "Vaccinated") %>%

      layout(title = chart_label(picker_state_var = picker_state_var(),
                                 picker_lga_var = picker_lga_var()),
             barmode = 'stack',
             xaxis = list(tickfont = font_plot(),
                          title = "Age group (in Months)",
                          fixedrange = TRUE,
                          title= font_axis_title(),
                          ticks = "outside",
                          showline = TRUE
             ),
             plot_bgcolor = measles_plot_bgcolor(),
             paper_bgcolor = measles_paper_bgcolor(),

             margin = plot_margin_one_side(),

             yaxis = list(side = 'left',
                          rangemode="tozero",
                          title = 'Number of cases',
                          showline = TRUE,
                          showgrid = FALSE,
                          fixedrange = TRUE,
                          zeroline = T,
                          ticks = "outside",
                          title = font_axis_title(),
                          tickfont = font_plot()),

             legend = list(orientation = "h",   # show entries horizontally
                           xanchor = "center",  # use center of legend as anchor
                           x = 0.5,
                           y = -0.25),
             hoverlabel = list(font = font_hoverlabel()),
             font = font_plot())%>%
      config(displayModeBar = FALSE)


    plot

  })




  output$plot <- renderPlotly({indicator_plot()})


  output$downloadData <- downloadHandler(

    filename = function() {
      paste0("Chart 2- Diphtheria", picker_state_var(), picker_lga_var(), picker_year_var(), picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".csv")
    },
    content = function(file) {
      readr::write_csv(chart_data(), file)
    }
  )


  output$downloadChart <- downloadHandler(
    filename = function() {
      paste0("Chart 2- Diphtheria", picker_state_var(),picker_lga_var(), picker_year_var(),  picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".png")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(indicator_plot(), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = file, cliprect = "viewport")

    }
  )


  })
}

## To be copied in the UI
# mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_ui("age_group_of_confirmed_diphtheria_cases_by_vaccination_status_1")

## To be copied in the server
# mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_server("age_group_of_confirmed_diphtheria_cases_by_vaccination_status_1")
