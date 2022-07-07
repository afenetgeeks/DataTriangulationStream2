#' national_measles_coverage_different_sources UI Function
#'
#' @description A shiny Module. slide 1
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmlwidgets saveWidget
mod_national_measles_coverage_different_sources_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t measles-col map_col",

   # div(class = "col-12 col-12-t measles-col",

        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/fully-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),
        HTML("<h6 class = 'column-title'> Chart 6: National <span class = 'measles-span'>MCV</span> Coverage (%) by different sources, Nigeria (National)</h6>"),


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

        withSpinner(plotlyOutput(ns("plot")),
                    type = 6, size = 0.3,hide.ui = F),


                p(style="text-align:center;", "***Note: This Chart is fixed, All the above filters don't affect the this chart (Chart 6)***"),


    )

  )
}

#' The application server-side
#'


#' national_measles_coverage_different_sources Server Functions
#' @importFrom dplyr collect tbl mutate arrange filter across group_by summarise ungroup
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config add_annotations export
#' @importFrom webshot webshot
#'
#' @noRd
mod_national_measles_coverage_different_sources_server <- function(id,
                                                                   picker_year_var,
                                                                   picker_month_var,
                                                                   picker_state_var
                                                                   ){


  moduleServer( id, function(input, output, session){

    ns <- session$ns

    # slide 1

    chart_data <- reactive({
        dplyr::tbl(stream2_pool, "mcv_different_sources") %>%
        dplyr::collect() %>%
        mutate(as.numeric(Year))

      })

indicator_plot <- reactive({

      fig <- plot_ly(chart_data())

      fig <- fig %>%
        add_trace(
          x = ~Year,y = ~`WUENIC (MCV1)`, type = 'scatter', mode = 'lines+markers',
          line = list(shape = 'spline', linetype = I("solid")),
          marker = list(symbol = I("circle")),
          color = I("#EE9B00"),
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          name = "*WUENIC (MCV1)")

      fig <- fig %>%
        add_trace(
          x = ~Year,
          y = ~NDHS,
          color = I("#0A9396"),
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          type = "bar",
          name = "*NDHS (MCV1)")

      fig <- fig %>%
        add_trace(
          x = ~Year,
          y = ~`SMART Survey`,
          color = I("#94D2BD"),
          type = "bar",
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          name = "*SMART Survey (MCV1)")

      fig <- fig %>%
        add_trace(
          x = ~Year,y = ~`NICS/MICS`,
          type = "bar",
          color = I("#00a5cf"),
          hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                                '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
          name = "*NICS/MICS (MCV1)")


      fig <- fig %>% add_trace(
        x = ~Year,
        y = ~`Dhis2 (MCV1)`,
        type = 'scatter',
        color = I("#E9D8A6"),
        mode = 'lines+markers',
        line = list(shape = 'spline', linetype = I("solid")),
        marker = list(symbol = I("circle")),
        hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                              '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
        name = "Dhis2 (MCV1)")

      fig <- fig %>% add_trace(
        x = ~Year,
        y = ~`Dhis2 (MCV2)`,
        type = 'scatter',
        color = I("#B37064"),
        mode = 'lines+markers',
        line = list(shape = 'spline', linetype = I("solid")),
        marker = list(symbol = I("circle")),
        hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                              '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
        name = "Dhis2 (MCV2)")

      fig <- fig %>% add_trace(
        x = ~Year,
        y = ~`WUENIC (MCV2)`,
        type = 'scatter',
        color = I("#0ce6ed"),
        mode = 'lines+markers',
        line = list(shape = 'spline', linetype = I("solid")),
        marker = list(symbol = I("circle")),
        hovertemplate = paste('<b>Coverage %</b>: %{y:.0f}',
                              '<br><b style="text-align:left;">Year</b>: %{x}<br>'),
        name = "*WUENIC (MCV2)")




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
        yref = "y",
        size = 14,
        color = "red",
        text = "85% National Coverage",
        xanchor = 'center',
        showarrow = F
      )

      fig <- fig %>%
        layout(xaxis = list(tickvals=chart_data()$Year,
                            tickfont = font_plot(),
                            title = "Years",
                            title= font_axis_title(),
                            ticks = "outside",
                            fixedrange = TRUE,
                            ticktext=chart_data()$Year),
               yaxis = list(title = "Coverage (%)",
                            ticks = "outside",
                            showline = TRUE,
                            fixedrange = TRUE,
                            range = c(0,100),
                            title = font_axis_title(),
                            tickfont = font_plot()),

               shapes = list(hline(y=95, color = "green", dash= "dash"),
                             hline(y=85, color = "black", dash= "dot")),

               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5,
                             y = -0.25),

               plot_bgcolor = measles_plot_bgcolor(),
               paper_bgcolor = measles_paper_bgcolor(),
               margin = plot_margin_one_side(),
               hoverlabel = list(font = font_hoverlabel()),
               font = font_plot())%>%
              config(displayModeBar = FALSE)

      fig

     })

  output$plot <- renderPlotly({indicator_plot()})


    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 6- National Measles Coverage by different sources-Nigeria National Wide-", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )



    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 6- National Measles Coverage by different sources-Nigeria National Wide.png")
      },
      content = function(file) {
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        saveWidget(indicator_plot(), "temp.html", selfcontained = FALSE)
        webshot("temp.html", file = file, cliprect = "viewport")
        #export(indicator_plot(), file=file)
      }
    )

  })
}

## To be copied in the UI
# mod_national_measles_coverage_different_sources_ui("national_measles_coverage_different_sources_1")

## To be copied in the server
# mod_national_measles_coverage_different_sources_server("national_measles_coverage_different_sources_1")
