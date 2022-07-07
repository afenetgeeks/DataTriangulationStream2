#' mcv1_mcv2_drop_out_rate_nigeria UI Function
#'
#' @description A shiny Module slide 8 -----
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_mcv1_mcv2_drop_out_rate_nigeria_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/total-registrations-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title'>Chart 5: MCV 1, MCV 2 Coverage & Drop Out Rate</h6>"),

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

#' mcv1_mcv2_drop_out_rate_nigeria Server Functions
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#' @importFrom dplyr collect tbl mutate arrange filter across
#' @noRd
mod_mcv1_mcv2_drop_out_rate_nigeria_server <- function(id,
                                                       picker_year_var,
                                                       picker_month_var,
                                                       picker_state_var,
                                                       picker_lga_var
                                                       ){
  moduleServer( id, function(input, output, session){

    ns <- session$ns


    chart_data <- reactive({

        dplyr::tbl(stream2_pool, "mcv1_mcv2_dropout_rate") %>%
        dplyr::filter(Year %in% !!picker_year_var() &
                      Months %in%  !!picker_month_var() &
                      State %in% !!picker_state_var() &
                      LGA %in% !!picker_lga_var()) %>%
        dplyr::collect() %>%
        dplyr::mutate(Months = as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),
                      dplyr::across(.col = c(Year,State ), as.factor)) %>%

        dplyr::arrange(Months)
      })

    indicator_plot <- reactive({


      min_rate <- min(chart_data()$`dropout_rate`,na.rm = T)

      max_rate <-  max(chart_data()$`first_dose`,na.rm = T)


      plotM12Dropout <- plot_ly(data = chart_data())

      plotM12Dropout <- plotM12Dropout %>% add_trace(x = ~Months,
                                                     y = ~`first_dose`,
                                                     type = 'bar',
                                                     name = 'MCV 1',
                                                     color = I("#005F73"),
                                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM12Dropout <- plotM12Dropout %>% add_trace(x = ~Months,
                                                     y = ~`second_dose`,
                                                     type = 'bar',
                                                     color = I("#00a5cf"),
                                                     name = 'MCV 2',
                                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM12Dropout <- plotM12Dropout %>% add_trace(x = ~Months,
                                                     y = ~`dropout_rate`,
                                                     type = 'scatter', mode = 'lines+markers',
                                                     line = list(shape = 'spline', linetype = I("solid")),
                                                     marker = list(symbol = I("circle")),
                                                     name = 'MCV 1 MCV 2 Dropout rate',
                                                     color = I("#edb952"),
                                                     hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotM12Dropout <- plotM12Dropout %>% layout( title = paste(picker_state_var(), "," ,picker_lga_var()),
                                                   xaxis = list(tickfont = font_plot(),
                                                                title = "Month",
                                                                fixedrange = TRUE,
                                                                title= font_axis_title(),
                                                                ticks = "outside",
                                                                tickvals = ~Months,
                                                                showline = TRUE,
                                                                dtick = "M1",
                                                                tickformat="%b-%Y"
                                                   ),

                                                   plot_bgcolor = measles_plot_bgcolor(),
                                                   paper_bgcolor = measles_paper_bgcolor(),


                                                   margin = plot_margin(),


                                                   yaxis = list(side = 'left',
                                                               #  range = if( (max_rate <= 100) & (min_rate <= 0)){ c(0 - min_rate - (min_rate/2), 100) }else{ c(min_rate - (min_rate/2) , max_rate+(max_rate/2))},
                                                                range =  plot_rate_range(min_rate, max_rate),
                                                                 overlaying = "y",
                                                                 fixedrange = TRUE,
                                                                 title = '(%)',
                                                                 showgrid = FALSE,
                                                                 ticks = "outside",
                                                                 zeroline = T,
                                                                 showline = TRUE, title = font_axis_title(), tickfont = font_plot()),

                                                   legend = list(orientation = "h",
                                                                 xanchor = "center",
                                                                 x = 0.5,
                                                                 y = -0.25),

                                                   hoverlabel = list(font = font_hoverlabel()),
                                                   font = font_plot())%>%
        config(displayModeBar = FALSE)

      plotM12Dropout


    })



    output$plot <- renderPlotly({indicator_plot()})



    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 5-",  picker_year_var(), picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".csv")
      },
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 5-",  picker_year_var(),  picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".png")
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
# mod_mcv1_mcv2_drop_out_rate_nigeria_ui("mcv1_mcv2_drop_out_rate_nigeria_1")

## To be copied in the server
# mod_mcv1_mcv2_drop_out_rate_nigeria_server("mcv1_mcv2_drop_out_rate_nigeria_1")
