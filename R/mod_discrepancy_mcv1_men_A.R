#'@rdname mod_discrepancy_mcv1_yellow_fever_given_by_state_ui
#'
mod_discrepancy_mcv1_men_A_ui <- function(id){
  ns <- NS(id)
  tagList(



    div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div meninigits-column-icon-div",
            img(class = "column-icon", src = "www/total-registrations-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title'>Chart 4: Co-administered Antigen discrepancy: MCV 1 & Men A given</h6>"),

        HTML(paste0('<a id="', ns("downloadData"), '" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>
                      <i class="fa fa-download" aria-hidden="true"></i>
                      <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>
                     </a>')),

        screenshotButton(id = ns("plot"), filename = "Chart 4 Co-administered Antigen discrepancy MCV 1 - Men A given", download =T, scale = 2, label = "", class = "download-data-btn download-chart-btn"),

         withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)

    )

  )
}


#' @rdname mod_discrepancy_mcv1_yellow_fever_given_by_state_server
#'
mod_discrepancy_mcv1_men_A_server <- function(id,
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

      dplyr::tbl(connection, "measles_men_A_discrepancy")%>%
        filter(Year %in% !!picker_year_var() &
                 Months %in%  !!picker_month_var() &
                 State %in% !!picker_state_var() &
                 LGA %in% !!picker_lga_var())%>%dplyr::collect()%>%
        dplyr::mutate(Months = as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),
                      dplyr::across(.col = c(Year,State), as.factor))%>%
        dplyr::arrange(Months)%>%
        mutate(discrepancy = abs(discrepancy))
    })

    indicator_plot <- reactive({

      min_max_rate <-  range(chart_data()$discrepancy,na.rm = T)

      min_max_number <- range(chart_data()$`main_vaccine_given`,na.rm = T)


      plotmcac <- plot_ly(data = chart_data())

      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~discrepancy,
                                         type = 'bar',
                                         color =  I("#00a5cf"),
                                         name = 'Discrepancy %',
                                         hovertemplate = paste('<b>Rate %</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      #
      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`main_vaccine_given`,
                                         color = I("#004e64"),
                                         mode = 'lines+markers',
                                         type = 'scatter',
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         name = 'MCV 1 given',
                                         hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                         yaxis = 'y2')

      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`other_vaccine_given`,
                                         color = I("#edb952"),
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         mode = 'lines+markers',
                                         type = 'scatter',
                                         hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                         name = 'Men A given',
                                         yaxis = 'y2')

      plotmcac <- plotmcac %>% layout(title =chart_label(picker_state_var = picker_state_var(),
                                                         picker_lga_var = picker_lga_var()),

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
                                      yaxis = list(side = 'left',
                                                   title = 'Discrepancy',
                                                   range = plot_rate_range(min_max_rate[1], min_max_rate[2]),
                                                   showline = TRUE,
                                                   rangemode="tozero",
                                                   showgrid = FALSE, zeroline = T, ticks = "outside",
                                                   title = font_axis_title(), tickfont =  font_axis_title()),

                                      yaxis2 = list(
                                        rangemode="tozero",
                                        side = 'right',
                                        range =  plot_number_range(min_max_number[1], min_max_number[2]),
                                        title = 'Men A & MCV 1 (given)',
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
                                      hoverlabel = list(font = font_hoverlabel()),
                                      font = font_plot())%>%
        config(displayModeBar = FALSE)

      plotmcac


    })

    output$plot <- renderPlotly({indicator_plot()})

    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 4 - Measles Men A discrepancy",   picker_state_var(), picker_lga_var() ,".csv")
      },
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )



  })
}

## To be copied in the UI
# mod_discrepancy_mcv1_men_A_ui("discrepancy_mcv1_men_A_1")

## To be copied in the server
# mod_discrepancy_mcv1_men_A_server("discrepancy_mcv1_men_A_1")
