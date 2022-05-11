#' confirmed_meningitis_cases_coverage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_confirmed_meningitis_cases_coverage_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-12 col-12-t measles-col",
   # div(class = "col-6 col-6-t measles-col",
        div(class ="column-icon-div measles-column-icon-div",
            img(class = "column-icon", src = "www/partially-vaccinated-today-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        # h6("Chart 2: Confirmed measles cases, MCV 1 coverage", class = "column-title"),

        HTML("<h6 class = 'column-title'>Chart 2: Confirmed <span class = 'measles-span'>Meningitis</span> cases, <span class = 'measles-span'>Meningitis</span> coverage </h6>"),

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

#' confirmed_meningitis_cases_coverage Server Functions
#'
#' @noRd
mod_confirmed_meningitis_cases_coverage_server <- function(id,
                                                           picker_year_var,
                                                           picker_month_var,
                                                           picker_state_var){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data <- reactive({

      dplyr::tbl(stream2_pool, "meningitis_coverage_cases_data")%>%
        dplyr::filter(Year %in% !!picker_year_var() & Months %in% !!picker_month_var() & State %in% !!picker_state_var())%>%
        collect() %>%
        mutate(Months = lubridate::month(as.Date(str_c(Year, Months, 01,sep = "-"), "%Y-%b-%d"), label = T),
               across(c(Year,State ), as.factor))

    })



    indicator_plot <- reactive({

      plotmcac <- plot_ly(data = chart_data() %>% arrange(Months))

      plotmcac <- plotmcac %>% add_trace(x = ~Months, y = ~`Meningitis Cases (CaseBased)`,
                                         type = 'bar',
                                         color =  I("#00a5cf"),
                                         name = 'Meningitis Cases (CaseBased)(Sormas)',
                                         #   marker = list(color = '#0000ff'),
                                         hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'))

      #
      plotmcac <- plotmcac %>% add_trace(x = ~Months,
                                         y = ~`Meningitis Coverage (Dhis2)`,
                                         color = I("#004e64"),
                                         mode = 'lines+markers', type = 'scatter',
                                         line = list(shape = 'spline', linetype = I("solid")),
                                         marker = list(symbol = I("circle")),
                                         name = 'Meningitis Coverage (Dhis2)',
                                         hovertemplate = paste('<b>Coverage %</b>: %{y:.1f}',
                                                               '<br><b style="text-align:left;">Month </b>: %{x}<br>'),
                                         yaxis = 'y2')


      plotmcac <- plotmcac %>% layout(
         title = list(text = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()),sep = "     "),
                      font = font_plot_title()),
        xaxis = list(tickfont = font_plot(),
                     title = "Month",
                     fixedrange = TRUE,
                     title= font_axis_title(),
                     ticks = "outside",
                     tickvals = ~Months,
                     showline = TRUE
        ),

        plot_bgcolor = measles_plot_bgcolor(),
        paper_bgcolor = measles_paper_bgcolor(),

        margin = plot_margin(),

        yaxis = list(side = 'left',
                     title = 'Number of cases',
                     showline = TRUE,
                     rangemode="tozero",
                     fixedrange = TRUE,
                     showgrid = FALSE,
                     zeroline = T,
                     ticks = "outside",
                     title = font_axis_title(), tickfont = font_plot()),
        yaxis2 = list(#range = c(0, 100),
          rangemode="tozero",
          fixedrange = TRUE,
          side = 'right',
          title = 'Coverage (%)',
          overlaying = "y",
          title = list(text = ""),
          showgrid = FALSE,
          ticks = "outside",
          zeroline = T,
          showline = TRUE,
          title = font_axis_title(),
          tickfont = font_plot()),



        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5,
                      y = -0.25),
        hoverlabel = list(font = font_hoverlabel()),
        font = font_plot())%>%
        config(displayModeBar = FALSE)
      # config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
      #        displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 2- Confirmed measles cases, MCV 1 coverage.png"))

      plotmcac
    })



    output$plot <- renderPlotly({indicator_plot()})



    output$downloadData <- downloadHandler(

      filename = function() {
        paste0("Chart 2-", picker_state_var(), picker_year_var(), picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".csv")
      },
      content = function(file) {
        readr::write_csv(rv$chart_data, file)
      }
    )


    output$downloadChart <- downloadHandler(
      filename = function() {
        paste0("Chart 2-", picker_state_var(), picker_year_var(),  picker_month_var()[1] ," - ", picker_month_var()[length(picker_month_var())] ,".png")
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
# mod_confirmed_meningitis_cases_coverage_ui("confirmed_meningitis_cases_coverage_1")

## To be copied in the server
# mod_confirmed_meningitis_cases_coverage_server("confirmed_meningitis_cases_coverage_1")
