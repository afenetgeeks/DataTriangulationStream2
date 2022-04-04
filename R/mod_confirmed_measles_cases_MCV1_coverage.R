#' confirmed_measles_cases_MCV1_coverage UI Function
#'
#' @description A shiny Module for slide 3
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_confirmed_measles_cases_MCV1_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t",
        div(class ="column-icon-div",
            img(class = "column-icon", src = "www/partially-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        h6("Chart 2: Confirmed measles cases, Measles 1 coverage", class = "column-title"),
        data_chart_download_btns(id),
        withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)

        )
    )
}

#' confirmed_measles_cases_MCV1_coverage Server Functions
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#' @importFrom dplyr collect tbl mutate arrange filter across
#' @noRd
mod_confirmed_measles_cases_MCV1_coverage_server <- function(id,
                                                             picker_year_var,
                                                             picker_month_var,
                                                             picker_state_var
                                                            ){

  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data_combined <- reactive({

      dplyr::tbl(stream2_pool, "s3_combined")%>%
        dplyr::filter(Year %in% !!picker_year_var() & Month %in% !!picker_month_var() & State %in% !!picker_state_var())%>%
        collect() %>%
        mutate(Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
               across(c(Year,State ), as.factor))

      })



    output$plot <- renderPlotly({

      plotmcac <- plot_ly(data = chart_data_combined() %>% arrange(Months))

      plotmcac <- plotmcac %>% add_trace(x = ~Months, y = ~`Measles Cases (CaseBased)`,
                                         type = 'bar',
                                         color =  I("#00a5cf"),
                                         name = 'Measles Cases (CaseBased) (Sormas)',
                                         #   marker = list(color = '#0000ff'),
                                         hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      #
      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`Measles Dose Coverage (Admin)`,
                                         color = I("#004e64"),
                                         mode = 'lines+markers', type = 'scatter',
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         name = 'Measles Dose Coverage (Admin) (Dhis2)',
                                         hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                         yaxis = 'y2')

      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`Measles Coverage (Alt Denominator)`,
                                         color = I("#edb952"),
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         mode = 'lines+markers', type = 'scatter',
                                         hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                         name = 'Measles Coverage (Alt Denominator) (Dhis2)',
                                         yaxis = 'y2')

      plotmcac <- plotmcac %>% layout(title = paste(paste0("State: ", picker_month_var()),paste0("Year: ", picker_year_var()), sep = "     "),
                                      title= list(size=10),
                                      xaxis = list(tickfont = font_plot(),
                                                   title = "Month",
                                                   #fixedrange = TRUE,
                                                   title= font_axis_title(),
                                                   ticks = "outside",
                                                   tickvals = ~Month,
                                                   showline = TRUE
                                      ),

                                      plot_bgcolor = "rgba(0, 0, 0, 0)",
                                      paper_bgcolor = 'rgba(0, 0, 0, 0)',

                                      margin = list(r = 85),

                                      yaxis = list(side = 'left', title = 'Number of cases',showline = TRUE, rangemode="tozero", showgrid = FALSE, zeroline = T, ticks = "outside",
                                                   title = font_axis_title(), tickfont = font_plot()),
                                      yaxis2 = list(#range = c(0, 100),
                                                    rangemode="tozero",
                                                    side = 'right', title = 'Coverage (%)', overlaying = "y", title = list(text = ""),showgrid = FALSE,ticks = "outside",
                                                    zeroline = T,showline = TRUE, title = font_axis_title(), tickfont = font_plot()),



                                      legend = list(orientation = "h",   # show entries horizontally
                                                    xanchor = "center",  # use center of legend as anchor
                                                    x = 0.5,
                                                    y = -0.25),
                                      hoverlabel = list(font = font_hoverlabel()),
                                      font = font_plot())%>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 2- Confirmed measles cases, Measles 1 coverage.png"))

      plotmcac

    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 2- Confirmed measles cases, Measles 1 coverage.csv",
      content = function(file) {
        readr::write_csv(chart_data_combined(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_confirmed_measles_cases_MCV1_coverage_ui("confirmed_measles_cases_MCV1_coverage_1")

## To be copied in the server
# mod_confirmed_measles_cases_MCV1_coverage_server("confirmed_measles_cases_MCV1_coverage_1")
