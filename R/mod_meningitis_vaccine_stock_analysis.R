#' meningitis_vaccine_stock_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_meningitis_vaccine_stock_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/vaccination-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title'>Chart 3: Men A vaccine stock analysis & Men A given</h6>"),

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

#' meningitis_vaccine_stock_analysis Server Functions
#'
#' @noRd
mod_meningitis_vaccine_stock_analysis_server <- function(id,
                                                         picker_year_var,
                                                         picker_month_var,
                                                         picker_state_var,
                                                         picker_lga_var){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data <- reactive({

      dplyr::tbl(stream2_pool, "men_A_stock_analysis")%>%
        dplyr::filter(Year %in% !!picker_year_var() &
                        Months %in% !!picker_month_var() &
                        State %in% !!picker_state_var() &
                        LGA %in% !!picker_lga_var()) %>%
        dplyr::collect() %>%
        dplyr::mutate(Months = as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),
                      dplyr::across(.col = c(Year,State ), as.factor)) %>%
        dplyr::arrange(Months)


    })

    indicator_plot <- reactive({
      # Men_A_OB_OU_Received_1_combined_by_Year

      min_max_rate <-  range(chart_data()$`Doses Wastage Rate`,na.rm = T)

      min_max_number <- range(chart_data()$`Doses Available (Opening Balance+Received)`,na.rm = T)


      plotM <- plot_ly(data = chart_data() %>%
                         arrange(Months))

      plotM <- plotM %>% add_trace(x = ~Months, y = ~`Doses Wastage Rate`,
                                   type = 'bar',
                                   color = I("#B37064"),
                                   hovertemplate = paste('<b>Men A Doses Wastage Rate %</b>: %{y:.1f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                   name = 'Men A Doses Wastage Rate',
                                   hoverinfo = "text",
                                   text = ~scales::number(`Doses Wastage Rate`,big.mark = ","))

      plotM <- plotM %>% add_trace(x = ~Months,
                                   y = ~`Vaccine - Doses Opened (used)`,
                                   color = I("#00a5cf"),
                                   type = 'scatter', mode = 'lines+markers',
                                   line = list(shape = 'spline', linetype = I("solid")),
                                   marker = list(symbol = I("circle")),
                                   name = 'Men A - Doses Opened (Used)',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),

                                   hoverinfo = "text",
                                   text = ~scales::number(`Vaccine - Doses Opened (used)`, big.mark = ","))

      plotM <- plotM %>% add_trace(x = ~Months,
                                   y = ~`doses_given`,
                                   color =  I("#94D2BD"),
                                   type = 'scatter',
                                   mode = 'lines+markers',
                                   line = list(shape = 'spline', linetype = I("solid")),
                                   marker = list(symbol = I("circle")),
                                   name = 'Men A given',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>Number</b>: %{y:.1f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                   text = ~scales::number(`doses_given`, big.mark = ",")

      )

      plotM <- plotM %>% add_trace(x = ~Months,
                                   y = ~`Doses Available (Opening Balance+Received)`,
                                   color = I("#edb952"),
                                   type = 'scatter',
                                   mode = 'lines+markers',
                                   line = list(shape = 'spline', linetype = I("solid")),
                                   marker = list(symbol = I("circle")),
                                   name = 'Men A Doses Available (Opening Balance+Received)',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                   text = ~scales::number(`Doses Available (Opening Balance+Received)`, big.mark = ","))

      plotM <- plotM %>% layout( title = paste(picker_state_var(), "," ,picker_lga_var()),
                                 xaxis = list(tickfont = font_plot(),
                                              title = "Month",
                                              fixedrange = TRUE,
                                              title= font_axis_title(),
                                              ticks = "outside",
                                              tickvals = ~ Months,
                                              showline = TRUE,
                                              dtick = "M1",
                                              tickformat="%b-%Y"

                                 ),



                                 plot_bgcolor = measles_plot_bgcolor(),
                                 paper_bgcolor = measles_paper_bgcolor(),

                                 margin = plot_margin(),


                                 yaxis = list(range = plot_rate_range(min_max_rate[1], min_max_rate[2]),
                                              side = 'left',
                                              title = 'Rate (%)',
                                              showline = TRUE,
                                              showgrid = FALSE,
                                              fixedrange = TRUE,
                                              zeroline = T,
                                              ticks = "outside",
                                              title = font_axis_title(), tickfont = font_plot()),


                                 yaxis2 = list(range =  plot_number_range(min_max_number[1], min_max_number[2]),
                                               side = 'right',
                                               overlaying = "y",
                                               rangemode="tozero",
                                               title = 'Number in doses',
                                               showgrid = FALSE,
                                               ticks = "outside",
                                               zeroline = FALSE,
                                               fixedrange = TRUE,
                                               showline = TRUE,
                                               title = font_axis_title(), tickfont = font_plot()),

                                 legend = list(orientation = "h",   # show entries horizontally
                                               xanchor = "center",  # use center of legend as anchor
                                               x = 0.5,
                                               y = -0.25),
                                 hoverlabel = list(font = font_hoverlabel()),
                                 font = font_plot())%>%
        config(displayModeBar = FALSE)

      plotM

    })


    output$plot <- renderPlotly({indicator_plot()})

    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 3-", picker_state_var(), picker_year_var(), picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".csv")
      },
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 3-", picker_state_var(), picker_year_var(),  picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".png")
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
# mod_meningitis_vaccine_stock_analysis_ui("meningitis_vaccine_stock_analysis_1")

## To be copied in the server
# mod_meningitis_vaccine_stock_analysis_server("meningitis_vaccine_stock_analysis_1")
