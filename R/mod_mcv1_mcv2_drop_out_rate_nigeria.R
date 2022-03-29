#' mcv1_mcv2_drop_out_rate_nigeria UI Function
#'
#' @description A shiny Module slide 8 -----
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_mcv1_mcv2_drop_out_rate_nigeria_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4("Chart 7: MCV1 & MCV2 Drop Out Rate, Nigeria",align = "center"),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)
        ))
    )

  )
}

#' mcv1_mcv2_drop_out_rate_nigeria Server Functions
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#'
#' @noRd
mod_mcv1_mcv2_drop_out_rate_nigeria_server <- function(id, picker_year_var,picker_month_var,picker_state_var){
  moduleServer( id, function(input, output, session){

    ns <- session$ns


    chart_data <- reactive({s8_combined %>%
      dplyr::filter(Year == picker_year_var() &
                      Month %in% picker_month_var())})


    output$plot <- renderPlotly({

      plotM12Dropout <- plot_ly(data = chart_data() %>% arrange(Months))

      plotM12Dropout <- plotM12Dropout %>% add_trace(x = ~Months, y = ~`Measles 1 given (administered)`,
                                                     type = 'bar',
                                                     name = 'MCV1',
                                                     color = I("#005F73"),
                                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM12Dropout <- plotM12Dropout %>% add_trace(x = ~Months, y = ~`Measles 2 given (administered)`,
                                                     type = 'bar',
                                                     color = I("#00a5cf"),
                                                     name = 'MCV2',
                                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM12Dropout <- plotM12Dropout %>% add_trace(x = ~Months,
                                                     y = ~`MCV 1 MCV 2 Droupout`,
                                                     type = 'scatter', mode = 'lines+markers',
                                                     line = list(shape = 'spline', linetype = I("solid")),
                                                     marker = list(symbol = I("circle")),
                                                     name = 'MCV 1 MCV 2 Droupout',
                                                     yaxis = 'y2',
                                                     color = I("#edb952"),
                                                     hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM12Dropout <- plotM12Dropout %>% layout( title = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()), sep = "     "),
                                                   title=list(size=10),
                                                   xaxis = list(tickfont = font,
                                                                title = "Month",
                                                                #fixedrange = TRUE,
                                                                title= font_axis_title,
                                                                ticks = "outside",
                                                                #type = 'date',
                                                                showline = TRUE,
                                                                tickvals = ~Months
                                                   ),
                                                   margin = list(r = 85),

                                                   #width = "auto",
                                                   # autosize = F,

                                                   plot_bgcolor = "rgba(0, 0, 0, 0)",
                                                   paper_bgcolor = 'rgba(0, 0, 0, 0)',

                                                   yaxis = list(side = 'left', rangemode="tozero", title = 'Measles Doses',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                                                                title = font_axis_title, tickfont = font),
                                                   yaxis2 = list(side = 'right',range = c(0, 100), overlaying = "y", title = 'Rate(%)',showgrid = FALSE,ticks = "outside",
                                                                 zeroline = FALSE,showline = TRUE, title = font_axis_title, tickfont = font),

                                                   legend = list(orientation = "h",
                                                                 xanchor = "center",
                                                                 x = 0.5,
                                                                 y = -0.25),

                                                   hoverlabel = list(font = font2),
                                                   font = font)%>%
        plotly::config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 7- MCV1 & MCV2 Drop Out Rate, Nigeria.png"))


      plotM12Dropout


    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 7- MCV1 & MCV2 Drop Out Rate, Nigeria.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_mcv1_mcv2_drop_out_rate_nigeria_ui("mcv1_mcv2_drop_out_rate_nigeria_1")

## To be copied in the server
# mod_mcv1_mcv2_drop_out_rate_nigeria_server("mcv1_mcv2_drop_out_rate_nigeria_1")
