
#' @rdname mod_measles_vaccine_stock_analysis_measles_coverage_ui
#'
mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t yf-col",
        div(class ="column-icon-div yf-column-icon-div",
            img(class = "column-icon", src = "www/vaccination-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        HTML("<h6 class = 'column-title'>Chart 3: Yellow Fever vaccine stock analysis & vaccine given</h6>"),

       HTML(paste0('<a id="', ns("downloadData"), '" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>
                      <i class="fa fa-download" aria-hidden="true"></i>
                      <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>
                     </a>')),

       screenshotButton(id = ns("plot"), filename = ">Chart 3- Yellow Fever vaccine stock analysis - vaccine given", download =T, scale = 2, label = "", class = "download-data-btn download-chart-btn"),
        withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)

    )

  )
}

#' @rdname mod_measles_vaccine_stock_analysis_measles_coverage_server
#'
mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server <- function(id,
                                                                                 picker_year_var,
                                                                                 picker_month_var,
                                                                                 picker_state_var,
                                                                                 picker_lga_var
                                                                                 ){

  stopifnot(is.reactive(picker_year_var))
  stopifnot(is.reactive(picker_month_var))
  stopifnot(is.reactive(picker_state_var))
  stopifnot(is.reactive(picker_lga_var))


  moduleServer( id, function(input, output, session){
    ns <- session$ns

    chart_data <- reactive({

      dplyr::tbl(connection, "yf_stock_analysis")%>%
        filter(Year %in% !!picker_year_var() &
                 Months %in% !!picker_month_var() &
                 State %in%  !! picker_state_var()  &
                 LGA %in% !!picker_lga_var()) %>%collect() %>%
        mutate(
               Months = as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"),
               across(c(Year,State ), as.factor)
               ) %>%
        arrange(Months)

      })

    indicator_plot <- reactive({

      min_max_rate <- range(chart_data()$`Doses Wastage Rate`,na.rm = T)

      min_max_number <- range(chart_data()$`Doses Available (Opening Balance+Received)`,na.rm = T)


      plotYF <- plot_ly(data = chart_data() %>% arrange(Months))


      plotYF <- plotYF %>% add_trace(x = ~Months,
                                     y = ~`Doses Wastage Rate`,
                                     type = 'bar',
                                   color = I("#B37064"),
                                   hovertemplate = paste('<b>Rate %</b>: %{y:.1f}',
                                                         '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                   name = 'Yellow Fever Doses Wastage Rate'
                                   )


      plotYF <- plotYF %>% add_trace(x = ~ Months,
                                     y = ~`Vaccine - Doses Opened (used)`,
                                     color = I("#00a5cf"),
                                     type = 'scatter',
                                     mode = 'lines+markers',
                                     line = list(shape = 'spline', linetype = I("solid")),
                                     marker = list(symbol = I("circle")),
                                     name = 'Yellow Fever Vaccine - Doses Opened (used)',
                                     yaxis = 'y2',
                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% add_trace(x = ~Months,
                                     y = ~`doses_given`,
                                     type = 'scatter',
                                     mode = 'lines+markers',
                                     line = list(shape = 'spline', linetype = I("solid")),
                                     marker = list(symbol = I("circle")),
                                     color = I("#94D2BD"),
                                     yaxis = 'y2',
                                     name = 'Yellow Fever given',
                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% add_trace(x = ~Months,
                                     y = ~`Doses Available (Opening Balance+Received)`,
                                     type = 'scatter',
                                     color = I("#edb952"),
                                     mode = 'lines+markers',
                                     yaxis = 'y2',
                                     line = list(shape = 'spline', linetype = I("solid")),
                                     marker = list(symbol = I("circle")),
                                     name = 'Yellow Fever Doses Available (Opening Balance+Received)',

                                     hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                           '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      plotYF <- plotYF %>% layout( title = chart_label(picker_state_var = picker_state_var(),
                                                       picker_lga_var = picker_lga_var()),


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


                                   #width = "auto",
                                   # autosize = F,

                                   plot_bgcolor = yf_plot_bgcolor(),
                                   paper_bgcolor = yf_paper_bgcolor(),
                                   margin = plot_margin(),

                                   yaxis = list(side = 'left',
                                                range = plot_rate_range(min_max_rate[1], min_max_rate[2]),
                                                title = 'Wastage Rate (%)',
                                                showline = TRUE,
                                                showgrid = FALSE,
                                                zeroline = T,
                                                fixedrange = TRUE,
                                                ticks = "outside",
                                                title = font_axis_title(), tickfont = font_plot()),
                                   yaxis2 = list(range =  plot_number_range(min_max_number[1], min_max_number[2]),
                                                 side = 'right',
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

      plotYF


    })

    output$plot <- renderPlotly({indicator_plot()})



    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 3- Yellow Fever", picker_state_var(), picker_year_var(),".csv")
      },
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
