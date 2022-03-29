#' measles_vaccine_stock_analysis_measles_coverage UI Function
#'
#' @description A shiny Module slide 6
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_measles_vaccine_stock_analysis_measles_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4("Chart 5: Measles Vaccine Stock Analysis & Measles Coverage",align = "center"),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)
        ))
    )


  )
}

#' measles_vaccine_stock_analysis_measles_coverage Server Functions
#'
#' @noRd
mod_measles_vaccine_stock_analysis_measles_coverage_server <- function(id,  picker_year_var, picker_month_var, picker_state_var){

  moduleServer( id, function(input, output, session){

    ns <- session$ns

    #
    chart_data <- reactive({
      s6_combined %>% dplyr::filter(Year == picker_year_var() & Month %in% picker_month_var() & State == picker_state_var())
      })


    output$plot <- renderPlotly({
      # measles_OB_OU_Received_1_combined_by_Year
      plotM <- plot_ly(data = chart_data() %>%
                         arrange(Months))

      plotM <- plotM %>% add_trace(x = ~Months, y = ~`Measles coverage`,
                                   type = 'bar',
                                   color = I("#004e64"),
                                   hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),

                                   name = 'Measles Coverage',
                                   #marker = list(color = '#ffa500'),
                                   hoverinfo = "text",
                                   text = ~scales::number(`Measles coverage`,big.mark = ","))

      plotM <- plotM %>% add_trace(x = ~Months,
                                   y = ~`Measles Vaccine - Doses Opened (used)`,
                                   color = I("#00a5cf"),
                                   type = 'scatter', mode = 'lines+markers',
                                   line = list(shape = 'spline', linetype = I("solid")),
                                   marker = list(symbol = I("circle")),
                                   name = 'Measles Vaccine - Doses Opened (Used)',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                   # line = list(color = '#0000ff'),
                                   hoverinfo = "text",
                                   text = ~scales::number(`Measles Vaccine - Doses Opened (used)`, big.mark = ","))

      plotM <- plotM %>% add_trace(x = ~Months,
                                   y = ~`Measles 1 given (administered)`,
                                   color =  I("#94D2BD"),
                                   type = 'scatter',
                                   mode = 'lines+markers',
                                   line = list(shape = 'spline', linetype = I("solid")),
                                   marker = list(symbol = I("circle")),
                                   name = 'MCV1 (administered)',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>Number</b>: %{y:.1f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>')
                                   # line = list(color = '#008000'),
      )

      plotM <- plotM %>% add_trace(x = ~Months,
                                   y = ~`Measles Doses Available (Opening Balance+Received)`,
                                   color = I("#edb952"),
                                   type = 'scatter',
                                   mode = 'lines+markers',
                                   line = list(shape = 'spline', linetype = I("solid")),
                                   marker = list(symbol = I("circle")),
                                   name = 'Measles Doses Available (Opening Balance+Received)',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM <- plotM %>% layout(title = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()), sep = "     "),
                                title= list(size=10),
                                xaxis = list(tickfont = font,
                                             title = "Month",
                                             title= font_axis_title,
                                             #fixedrange = TRUE,
                                             ticks = "outside",
                                             showline = TRUE,
                                             tickvals = ~Months),

                                #width = "auto",
                                # autosize = F,

                                plot_bgcolor = "rgba(0, 0, 0, 0)",
                                paper_bgcolor = 'rgba(0, 0, 0, 0)',



                                yaxis = list(#range = c(0, 100),
                                             side = 'right', title = 'Coverage (%)',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                                             title = font_axis_title, tickfont = font),
                                yaxis2 = list(side = 'left', overlaying = "y", rangemode="tozero", title = 'Vaccine Stock (number in doses)',showgrid = FALSE,ticks = "outside",
                                              zeroline = FALSE,showline = TRUE, title = font_axis_title, tickfont = font),

                                legend = list(orientation = "h",   # show entries horizontally
                                              xanchor = "center",  # use center of legend as anchor
                                              x = 0.5,
                                              y = -0.25),
                                hoverlabel = list(font = font2),
                                font = font)%>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 5- Measles Vaccine Stock Analysis & Measles Coverage.png"))
      plotM

    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 5- Measles Vaccine Stock Analysis & Measles Coverage.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_measles_vaccine_stock_analysis_measles_coverage_ui("measles_vaccine_stock_analysis_measles_coverage_1")

## To be copied in the server
# mod_measles_vaccine_stock_analysis_measles_coverage_server("measles_vaccine_stock_analysis_measles_coverage_1")
