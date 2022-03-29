#' discrepancy_mcv1_yellow_fever_given_by_state UI Function
#'
#' @description A shiny Module slide 10
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile  f7Col  f7Shadow  f7DownloadButton
mod_discrepancy_mcv1_yellow_fever_given_by_state_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4("Chart 9: Discrepancy (MCV1 & Yellow Fever given)  by State",align = "center"),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F))
      )
    )

  )
}

#' discrepancy_mcv1_yellow_fever_given_by_state Server Functions
#'
#' @noRd
mod_discrepancy_mcv1_yellow_fever_given_by_state_server <- function(id, picker_year_var,picker_month_var,picker_state_var){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    chart_data <- reactive({s10_combined %>%
      filter(Year %in% picker_year_var() &
               Months %in% picker_month_var())})


    output$plot <- renderPlotly({

      plotM <- plot_ly(data = chart_data() %>%
                         dplyr::mutate(State = fct_reorder(State,`Measles 1 given`, .desc = TRUE)) )

      plotM <- plotM %>% add_trace(x = ~State,
                                   y = ~`Measles 1 given`,
                                   type = 'bar',
                                   color = I("#005F73"),

                                   name = 'MCV1',
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

      plotM <- plotM %>% layout(title = paste0("Year: ", picker_year_var()),
                                title=list(size=10),
                                xaxis = list(title = "States",tickfont = font,
                                             #fixedrange = TRUE,

                                             title= font_axis_title,
                                             ticks = "outside",showline = T,
                                             title = list(size = 12), tickangle=-45, tickfont = list(size = 12)),
                                margin = list(r = 82),


                                plot_bgcolor = "rgba(0, 0, 0, 0)",
                                paper_bgcolor = 'rgba(0, 0, 0, 0)',


                                yaxis = list(side = 'left', title = 'Number of Doses.',rangemode="tozero",showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                                             title = font_axis_title, tickfont = font),
                                yaxis2 = list(#range = c(0, 100),
                                              rangemode="tozero",
                                              side = 'right', overlaying = "y", title = 'Rate (%)',showgrid = FALSE,ticks = "outside",
                                              zeroline = FALSE,showline = TRUE, title = font_axis_title, tickfont = font),


                                legend = list(orientation = "h",   # show entries horizontally
                                              xanchor = "center",  # use center of legend as anchor
                                              x = 0.5,y=-0.6),
                                hoverlabel = list(font = font2),
                                font = font)%>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 9- Discrepancy (MCV1 & Yellow Fever given)  by State.png"))

      plotM

    })


    output$download_chart_data <- downloadHandler(
      filename = "Chart 9- Discrepancy (MCV1 & Yellow Fever given)  by State.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


  })
}

## To be copied in the UI
# mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("discrepancy_mcv1_yellow_fever_given_by_state_1")

## To be copied in the server
# mod_discrepancy_mcv1_yellow_fever_given_by_state_server("discrepancy_mcv1_yellow_fever_given_by_state_1")
