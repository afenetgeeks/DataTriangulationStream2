#' @rdname mod_confirmed_measles_cases_MCV1_coverage_ui
#'
mod_meningitis_coverage_confirmed_cases_ui <- function(id){
  ns <- NS(id)
  tagList(


    div(class = "col-6 col-6-t yf-col",
        div(class ="column-icon-div meninigits-column-icon-div",
            img(class = "column-icon", src = "www/partially-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),


        HTML("<h6 class = 'column-title'> Chart 1: Confirmed meningitis cases and coverage</h6>"),

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

        withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)

    )


  )
}

#' @rdname mod_confirmed_measles_cases_MCV1_coverage_server
#'
#'
mod_meningitis_coverage_confirmed_cases_server <- function(id,
                                                           picker_year_var,
                                                           picker_month_var,
                                                           picker_state_var,
                                                           picker_lga_var){

  stopifnot(is.reactive(picker_year_var))
  stopifnot(is.reactive(picker_month_var))
  stopifnot(is.reactive(picker_state_var))
  stopifnot(is.reactive(picker_lga_var))

  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data <- reactive({

      dplyr::tbl(connection, "men_A_alt_denominator2")%>%
        dplyr::filter(Year %in% !!picker_year_var() &
                        Months %in% !!picker_month_var() &
                        State %in% !!picker_state_var() &
                        LGA %in% !!picker_lga_var())%>% collect() %>%
        mutate(Months = as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),
               across(c(Year,State ), as.factor))
    })



    indicator_plot <- reactive({

      min_max_rate <- range(chart_data()$`Disease Alt Denominator`,na.rm = T)

      min_max_number <-  range(chart_data()$`Disease Cases`,na.rm = T)


      plotmcac <- plot_ly(data = chart_data() %>% arrange(Months))

      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`Disease Coverage`,
                                         yaxis = 'y2',
                                         color = I("#004e64"),
                                         mode = 'lines+markers', type = 'scatter',
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         name = 'Meningitis Coverage (DHIS2)',
                                         hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>')
      )


      plotmcac <- plotmcac %>% add_trace(x = ~ Months,
                                         y = ~ `Disease Alt Denominator`,
                                         yaxis = 'y2',
                                         color = I("#edb952"),
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         mode = 'lines+markers', type = 'scatter',
                                         hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                         name = 'Meningitis Alt Denominator')


      plotmcac <- plotmcac %>% add_trace(x = ~ Months,
                                         y = ~ `Disease Cases`,
                                         type = 'bar',
                                         color =  I("#00a5cf"),
                                         name = 'Meningitis Cases (Sormas)',

                                         hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>')

      )

      plotmcac <- plotmcac %>% layout(title = chart_label(picker_state_var = picker_state_var(),
                                                          picker_lga_var = picker_lga_var()),

                                      xaxis = list(tickfont = font_plot(),
                                                   title = "Month",
                                                   fixedrange = TRUE,
                                                   title= font_axis_title(),
                                                   ticks = "outside",
                                                   tickvals = ~ Months,
                                                   showline = TRUE,
                                                   dtick = "M1",
                                                   tickformat="%b-%Y"),

                                      plot_bgcolor = measles_plot_bgcolor(),
                                      paper_bgcolor = measles_paper_bgcolor(),

                                      margin = plot_margin(),

                                      yaxis2 = list(range = plot_rate_range(min_max_rate[1], min_max_rate[2]),
                                                    rangemode="tozero",
                                                    fixedrange = TRUE,
                                                    side = 'left',
                                                    title = 'Coverage (%)',
                                                    overlaying = "y",
                                                    showgrid = FALSE,
                                                    ticks = "outside",
                                                    zeroline = T,
                                                    showline = TRUE,
                                                    title = font_axis_title(),
                                                    tickfont = font_plot()),

                                      yaxis = list(range = plot_number_range(min_max_number[1], min_max_number[2]),
                                                   side = 'right',
                                                   title = 'Number of cases',
                                                   showline = TRUE,
                                                   rangemode="tozero",
                                                   fixedrange = TRUE,
                                                   showgrid = FALSE,
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

      plotmcac

    })



    output$plot <- renderPlotly({indicator_plot()})



    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 1- Meningitis", picker_state_var(), picker_lga_var() ,".csv")
      },
      content = function(file) {

        readr::write_csv(chart_data(), file)
      }
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 1- Meningitis", picker_state_var(), picker_lga_var(),".png")
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
# mod_meningitis_coverage_confirmed_cases_ui("meningitis_coverage_confirmed_cases_1")

## To be copied in the server
# mod_meningitis_coverage_confirmed_cases_server("meningitis_coverage_confirmed_cases_1")
