#' single_plot_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyMobile
mod_single_plot_row_ui <- function(id, chart_title = "Chart"){
  ns <- NS(id)
  tagList(

    f7Row(
      f7Col(
        f7Shadow(
          intensity = 4,
          hover = TRUE,
          f7Card(
            title = NULL,
            splitLayout(h4(chart_title, align = "center"),
                        f7DownloadButton(ns("download_ch1Data"), label = NULL),
                        cellWidths = c("95%", "5%")),
            withSpinner(plotlyOutput(ns("slide1")),type = 6, size = 0.3,hide.ui = F)


          )))
    )

  )
}

#' single_plot_row Server Functions
#'
#' @noRd
mod_single_plot_row_server <- function(id){
  moduleServer( id, function(input, output, session, picker_year_var, picker_month_var, picker_state_var){
    ns <- session$ns

    measles_coverage <- reactive({
      slide1_data
    })



    output$slide1 <- renderPlotly({

      fig <- plot_ly(measles_coverage())

      fig <- fig %>% add_trace(
        color = I("#005F73"),
        x = ~Year,
        y = ~PMCCS,
        type = "bar",
        hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                              '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
        name = "*PMCCS")
      fig <- fig %>% add_trace(
        x = ~Year,y = ~`Admin (DHIS2)`,
        type = 'scatter',
        color = I("#E9D8A6"),
        mode = 'lines+markers',
        line = list(shape = 'spline', linetype = I("solid")),
        marker = list(symbol = I("circle")),
        hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                              '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
        name = "Admin (DHIS2)")
      fig <- fig %>%
        add_trace(
          x = ~Year,y = ~NDHS,
          color = I("#0A9396"),
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          type = "bar",
          name = "*NDHS")
      fig <- fig %>%
        add_trace(
          x = ~Year,y = ~`SMART Survey`,
          color = I("#94D2BD"),
          type = "bar",
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          name = "*SMART Survey")
      fig <- fig %>%
        add_trace(
          x = ~Year,y = ~`NICS/MICS`,
          type = "bar",
          color = I("#00a5cf"),
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          name = "*NICS/MICS")
      fig <- fig %>%
        add_trace(
          x = ~Year,y = ~WUENIC, type = 'scatter', mode = 'lines+markers',
          line = list(shape = 'spline', linetype = I("solid")),
          marker = list(symbol = I("circle")),
          color = I("#EE9B00"),
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          name = "*WUENIC")



      hline <- function(y = 0, color = "blue", dash = "dash") {
        list(
          type = "line",
          x0 = 0,
          x1 = 1,
          xref = "paper",
          y0 = y,
          y1 = y,
          line = list(width = 2, dash= dash, color = color)
        )
      }

      # xaxis = list(title = list(size = 5), tickfont = list(size = 5))


      fig <- fig %>% add_annotations(
        x=2010,
        bgcolor = "#04ec04",
        y=95,
        xref = "x", size = 14,
        color="red",
        yref = "y",
        text = "95% Global target for <br> measles elimination",
        xanchor = 'center',
        showarrow = F
      )
      fig <- fig %>% add_annotations(
        x=2010, bgcolor = "yellow",
        y=85,
        xref = "x",
        yref = "y", size = 14,
        color = "red",
        text = "85% National Coverage",
        xanchor = 'center',
        showarrow = F
      )

      fig <- fig %>%
        layout(xaxis = list(tickvals=measles_coverage()$Year,

                            tickfont = font,
                            title = "Years",
                            title= font_axis_title,
                            ticks = "outside",
                            #fixedrange = TRUE,
                            ticktext=measles_coverage()$Year),
               yaxis = list(title = "Coverage (%)",
                            ticks = "outside",
                            showline = TRUE,
                            range = c(0,100),
                            title = font_axis_title,
                            tickfont = font),

               shapes = list(hline(y=95, color = "green", dash= "dash"),
                             hline(y=85, color = "black", dash= "dot")),

               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5,
                             y = -0.25),

               plot_bgcolor = "rgba(0, 0, 0, 0)",
               paper_bgcolor = 'rgba(0, 0, 0, 0)',
               hoverlabel = list(font = font2),
               font = font)%>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 1- National Measles Coverage (%) by different sources, Nigeria (National Wide).png"))
      fig

    })

    output$download_ch1Data <- downloadHandler(
      filename = "Chart 1- National Measles Coverage (%) by different sources, Nigeria (National Wide).csv",
      content = function(file) {
        readr::write_csv(measles_coverage(), file)
      }
    )


  })
}

## To be copied in the UI
# mod_single_plot_row_ui("single_plot_row_1")

## To be copied in the server
# mod_single_plot_row_server("single_plot_row_1")
