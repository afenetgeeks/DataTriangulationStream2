#' yellow_fever_vaccine_stock_analysis_yellow_fever_coverage UI Function
#'
#' @description A shiny Module. slide 7 ------
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t yf-col",
        div(class ="column-icon-div yf-column-icon-div",
            img(class = "column-icon", src = "www/vaccination-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

       # h6("Chart 6: Yellow Fever Vaccine Stock Analysis & Yellow Fever Coverage", class = "column-title"),

        HTML("<h6 class = 'column-title'>Chart 6: <span class = 'yf-span'>Yellow Fever</span> Vaccine Stock Analysis & <span class = 'yf-span'>Yellow Fever</span> Coverage (Annual data)</h6>"),

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

#' yellow_fever_vaccine_stock_analysis_yellow_fever_coverage Server Functions
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#' @importFrom dplyr collect tbl mutate arrange filter across
#' @noRd
mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server <- function(id,
                                                                                 picker_year_var,
                                                                                 picker_month_var,
                                                                                 picker_state_var
                                                                                 ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    chart_data <- reactive({

      dplyr::tbl(stream2_pool, "s7_combined")%>%
        filter(Year == !!picker_year_var() &
                 Month %in% !!picker_month_var() &
                 State == !! picker_state_var()) %>%collect() %>%
        mutate(`Yellow Fever Coverage` = `Yellow Fever Coverage`*100,
               Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
               across(c(Year,State ), as.factor)) %>%
        arrange(Months)

      })

    indicator_plot <- reactive({


      plotYF <- plot_ly(data = chart_data() %>% arrange(Months))

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

      plotYF <- plotYF %>% layout( title = list(text = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()), sep = "     "),
                                                font = font_plot_title()),
                                   xaxis = list(tickfont = font_plot(),
                                                title = "Month",
                                                fixedrange = TRUE,
                                                title= font_axis_title(),
                                                ticks = "outside",
                                                showline = TRUE,
                                                ticktext = list("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                                tickvals = ~Months),


                                   #width = "auto",
                                   # autosize = F,

                                   plot_bgcolor = yf_plot_bgcolor(),
                                   paper_bgcolor = yf_paper_bgcolor(),
                                   margin = plot_margin(),

                                   yaxis = list(side = 'right',
                                                #range = c(0, 100),
                                                title = 'Coverage (%)',
                                                showline = TRUE,
                                                showgrid = FALSE,
                                                zeroline = T,
                                                fixedrange = TRUE,
                                                ticks = "outside",
                                                title = font_axis_title(), tickfont = font_plot()),
                                   yaxis2 = list(side = 'left',
                                                 rangemode="tozero",
                                                 overlaying = "y",
                                                 title = 'Number',
                                                 showgrid = FALSE,
                                                 ticks = "outside",
                                                 zeroline = FALSE,
                                                 fixedrange = TRUE,
                                                 showline = TRUE,
                                                 title = font_axis_title(), tickfont = font_plot()),

                                   legend = list(orientation = "h",
                                                 xanchor = "center",
                                                 x = 0.5,
                                                 y = -0.25),

                                   hoverlabel = list(font = font_hoverlabel()),
                                   font = font_plot())%>%
        config(displayModeBar = FALSE)

      # config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
      #        displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 6- Yellow Fever Vaccine Stock Analysis & Yellow Fever Coverage.png"))


      plotYF


    })

    output$plot <- renderPlotly({indicator_plot()})



    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 6-", picker_state_var(), picker_year_var(), picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".csv")
      },
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 6-", picker_state_var(), picker_year_var(),  picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".png")
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
# mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1")

## To be copied in the server
# mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1")
