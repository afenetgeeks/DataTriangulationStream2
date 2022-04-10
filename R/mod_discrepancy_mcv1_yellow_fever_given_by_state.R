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

      # h6("Chart 9: Discrepancy (Measles 1 & Yellow Fever given)  by State", class = "column-title"),

        HTML("<h6 class = 'column-title'>Chart 9: Discrepancy (<span class = 'measles-span'>Measles 1</span> & <span class = 'yf-span'>Yellow Fever</span> given )  by State </h6>"),

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
                                                                    picker_state_var
                                                                    ){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data <- reactive({
      # slide 10
      dplyr::tbl(stream2_pool, "s10_combined")%>%
        filter(Year %in% !!picker_year_var() &
                 Months %in%  !!picker_month_var())%>%dplyr::collect()%>%
        dplyr::mutate(dplyr::across(.col = c(Year,State, Months ), as.factor))
})

    indicator_plot <- reactive({


      plotM <- plot_ly(data = chart_data() %>%
                         dplyr::mutate(State = fct_reorder(State,`Measles 1 given`, .desc = TRUE)) )

      plotM <- plotM %>% add_trace(x = ~State,
                                   y = ~`Measles 1 given`,
                                   type = 'bar',
                                   color = I("#005F73"),

                                   name = 'Measles 1',
                                   hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                         '<br><b style="text-align:left;">State </b>: %{x}<br>'))

      plotM <- plotM %>% add_trace(x = ~State,
                                   y = ~`Yellow Fever given`,
                                   type = 'bar',
                                   color = I("#00a5cf"),
                                   name = 'Yellow Fever given',
                                   hovertemplate = paste('<b>Number</b>: %{y:.0f}',
                                                         '<br><b style="text-align:left;">State </b>: %{x}<br>'))

      plotM <- plotM %>% add_trace(x = ~State,
                                   y = ~Discrepancy,
                                   color = I("#edb952"),
                                   type = 'scatter', mode = 'markers',
                                   name = 'Discrepancy',
                                   yaxis = 'y2',
                                   hovertemplate = paste('<b>%</b>: %{y:.1f}',
                                                         '<br><b style="text-align:left;">State </b>: %{x}<br>'))

      plotM <- plotM %>% layout( title = list(text =  paste0("Year: ", picker_year_var()),
                                              font = font_plot_title()),
                                 xaxis = list(title = "States",
                                              tickfont = font_plot(),
                                              fixedrange = TRUE,
                                              title= font_axis_title(),
                                              ticks = "outside",
                                              showline = T,
                                              tickangle=-45
                                 ),



                                 plot_bgcolor = measles_plot_bgcolor(),
                                 paper_bgcolor = measles_paper_bgcolor(),
                                 margin = plot_margin(),


                                 yaxis = list(side = 'left',
                                              title = 'Number of Doses.',
                                              rangemode="tozero",
                                              showline = TRUE,
                                              showgrid = FALSE,
                                              fixedrange = TRUE,
                                              zeroline = T,
                                              ticks = "outside",
                                              title = font_axis_title(), tickfont = font_plot()),
                                 yaxis2 = list(#range = c(0, 100),
                                   rangemode="tozero",
                                   side = 'right',
                                   overlaying = "y",
                                   fixedrange = TRUE,
                                   title = 'Rate (%)',
                                   showgrid = FALSE,ticks = "outside",
                                   zeroline = FALSE,showline = TRUE, title = font_axis_title(), tickfont = font_plot()),


                                 legend = list(orientation = "h",   # show entries horizontally
                                               xanchor = "center",  # use center of legend as anchor
                                               x = 0.5,y=-0.6),
                                 hoverlabel = list(font = font_hoverlabel()),
                                 font = font_plot())%>%
        config(displayModeBar = FALSE)

      # config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
      #        displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 9- Discrepancy (Measles 1 & Yellow Fever given)  by State.png"))

      plotM

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
