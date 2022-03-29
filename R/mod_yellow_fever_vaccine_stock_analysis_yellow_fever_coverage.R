#' yellow_fever_vaccine_stock_analysis_yellow_fever_coverage UI Function
#'
#' @description A shiny Module. slide 7 ------
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4("Chart 6: Yellow Fever Vaccine Stock Analysis & Yellow Fever Coverage",align = "center"),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F),
        ))
    )
  )
}

#' yellow_fever_vaccine_stock_analysis_yellow_fever_coverage Server Functions
#'
#' @noRd
mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server <- function(id, picker_year_var,picker_month_var,picker_state_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    chart_data <- reactive({s7_combined %>%
      dplyr::filter(Year == picker_year_var() &
                      Month %in% picker_month_var() &
                      State == picker_state_var()) %>%
      mutate(Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"))) %>%
      tibble() %>% arrange(Months)})

    output$plot <- renderPlotly({

      plotYF <- plot_ly(data = chart_data())

      plotYF <- plotYF %>% add_trace(x = ~Months, y = ~`Yellow Fever Coverage`,
                                     type = 'bar',
                                     color = I("#005F73"),
                                     name = 'Yellow Fever Coverage',
                                     #marker = list(color = '#ffa500'),
                                     hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% add_trace(x = ~ Months,
                                     y = ~`Yellow Fever Vaccine - Doses Opened (used)`,
                                     color = I("#00a5cf"),
                                     type = 'scatter',
                                     mode = 'lines+markers',
                                     line = list(shape = 'spline', linetype = I("solid")),
                                     marker = list(symbol = I("circle")),
                                     name = 'Yellow Fever Vaccine - Doses Opened (used)',
                                     yaxis = 'y2',
                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% add_trace(x = ~Months, y = ~`Yellow Fever given (administered)`, type = 'scatter',
                                     mode = 'lines+markers',
                                     line = list(shape = 'spline', linetype = I("solid")),
                                     marker = list(symbol = I("circle")),
                                     color = I("#94D2BD"),
                                     name = 'Yellow Fever given (administered)',
                                     yaxis = 'y2',
                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% add_trace(x = ~Months,
                                     y = ~`Yellow Fever Doses Available (Opening Balance+Received)`,
                                     type = 'scatter',
                                     color = I("#edb952"),
                                     mode = 'lines+markers',
                                     line = list(shape = 'spline', linetype = I("solid")),
                                     marker = list(symbol = I("circle")),
                                     name = 'Yellow Fever Doses Available (Opening Balance+Received)',
                                     yaxis = 'y2',
                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% layout( title = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()), sep = "     "),
                                   title=list(size=10),
                                   xaxis = list(tickfont = font,
                                                title = "Month",
                                                #fixedrange = TRUE,
                                                title= font_axis_title,
                                                ticks = "outside",
                                                showline = TRUE,
                                                ticktext = list("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                                tickvals = ~Months),
                                   margin = list(r = 85),

                                   #width = "auto",
                                   # autosize = F,

                                   plot_bgcolor = "rgba(0, 0, 0, 0)",
                                   paper_bgcolor = 'rgba(0, 0, 0, 0)',

                                   yaxis = list(side = 'right',
                                                #range = c(0, 100),
                                                title = 'Coverage (%)',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                                                title = font_axis_title, tickfont = font),
                                   yaxis2 = list(side = 'left', rangemode="tozero", overlaying = "y", title = 'Number',showgrid = FALSE,ticks = "outside",
                                                 zeroline = FALSE,showline = TRUE, title = font_axis_title, tickfont = font),

                                   legend = list(orientation = "h",
                                                 xanchor = "center",
                                                 x = 0.5,
                                                 y = -0.25),

                                   hoverlabel = list(font = font2),
                                   font = font)%>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 6- Yellow Fever Vaccine Stock Analysis & Yellow Fever Coverage.png"))


      plotYF


    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 6- Yellow Fever Vaccine Stock Analysis & Yellow Fever Coverage.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )



  })
}

## To be copied in the UI
# mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1")

## To be copied in the server
# mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1")
