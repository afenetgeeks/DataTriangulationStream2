#' discrepancy_mcv1_yellow_fever_given_by_state UI Function
#'
#' @description A shiny Module slide 10
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_discrepancy_mcv1_yellow_fever_given_by_state_ui <- function(id){
  ns <- NS(id)
  tagList(



    div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/total-registrations-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

      # h6("Chart 9: Discrepancy (MCV 1 & Yellow Fever given)  by State", class = "column-title"),

        HTML("<h6 class = 'column-title'>Chart 6(Measles) 3 (Yellow Fever): Discrepancy (<span class = 'measles-span'>MCV 1</span> & <span class = 'yf-span'>Yellow Fever</span> given )  by State </h6>"),

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

#' discrepancy_mcv1_yellow_fever_given_by_state Server Functions
#'
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#' @importFrom dplyr collect tbl mutate arrange filter across
#' @importFrom forcats fct_reorder
#'
#' @noRd
mod_discrepancy_mcv1_yellow_fever_given_by_state_server <- function(id,
                                                                    picker_year_var,
                                                                    picker_month_var,
                                                                    picker_state_var,
                                                                    picker_lga_var
                                                                    ){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data <- reactive({
      # slide 10
      dplyr::tbl(stream2_pool, "s10_combined")%>%
        filter(Year %in% !!picker_year_var() &
                 Months %in%  !!picker_month_var() &
                State %in% !!picker_state_var() &
                 LGA %in% !!picker_lga_var())%>%dplyr::collect()%>%
        dplyr::mutate(Months = as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),
                      dplyr::across(.col = c(Year,State), as.factor))%>%
        dplyr::arrange(Months)
})

    indicator_plot <- reactive({


      plotmcac <- plot_ly(data = chart_data())

      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~Discrepancy,
                                         type = 'bar',
                                         color =  I("#00a5cf"),
                                         name = 'Discrepancy %',
                                         #   marker = list(color = '#0000ff'),
                                         hovertemplate = paste('<b style="text-align:left;>Value</b>: %{y:.0f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      #
      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`Measles 1 given`,
                                         color = I("#004e64"),
                                         mode = 'lines+markers',
                                         type = 'scatter',
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         name = 'Measles 1 given',
                                         hovertemplate = paste('<b>Measles 1 given/b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Months </b>: %{x}<br>'),
                                         yaxis = 'y2')

      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`Yellow Fever given`,
                                         color = I("#edb952"),
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         mode = 'lines+markers',
                                         type = 'scatter',
                                         hovertemplate = paste('<b>Yellow Fever given</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Months </b>: %{x}<br>'),
                                         name = 'Yellow Fever given',
                                         yaxis = 'y2')

      plotmcac <- plotmcac %>% layout(title = paste(picker_state_var(), "," ,picker_lga_var()),
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


                                      margin = list(r = 85),

                                      yaxis = list(side = 'right',
                                                   title = 'Discrepancy',
                                                   range = if(max(chart_data()$Discrepancy, na.rm = T) <= 100){c(0, 100)}else{NULL},
                                                   showline = TRUE,
                                                   rangemode="tozero",
                                                   showgrid = FALSE, zeroline = T, ticks = "outside",
                                                   title = font_axis_title(), tickfont =  font_axis_title()),

                                      yaxis2 = list(
                                        rangemode="tozero",
                                        side = 'left',
                                        title = 'Yellow Fever & Measles 1 (given)',
                                        overlaying = "y",
                                        showgrid = FALSE,
                                        ticks = "outside",
                                        zeroline = T,
                                        showline = TRUE,
                                        title = font_axis_title(),
                                        tickfont =  font_axis_title()),
                                        legend = list(orientation = "h",   # show entries horizontally
                                                    xanchor = "center",  # use center of legend as anchor
                                                    x = 0.5,
                                                    y = -0.25),
                                      hoverlabel = list(font = font_plot()),
                                      font = font_plot())%>%
        config(displayModeBar = FALSE)

      plotmcac


    })

    output$plot <- renderPlotly({indicator_plot()})

    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 9-",  picker_year_var(), picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".csv")
      },
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 9-",  picker_year_var(),  picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".png")
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
# mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("discrepancy_mcv1_yellow_fever_given_by_state_1")

## To be copied in the server
# mod_discrepancy_mcv1_yellow_fever_given_by_state_server("discrepancy_mcv1_yellow_fever_given_by_state_1")
