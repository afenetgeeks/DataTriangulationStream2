#' mcv1_mcv2_drop_out_rate UI Function
#'
#' @description A shiny Module slide 9 -----
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mcv1_mcv2_drop_out_rate_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4("Chart 8: MCV1 and MCV2 Drop Out Rate", align = "center"),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F))
      ))

  )
}

#' mcv1_mcv2_drop_out_rate Server Functions
#'@importFrom forcats fct_reorder
#'
#' @noRd
mod_mcv1_mcv2_drop_out_rate_server <- function(id,picker_year_var,picker_month_var,picker_state_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    chart_data <- reactive({s9_combined %>%
                                          filter(Year %in% picker_year_var() &
                                                  Months %in% picker_month_var())})

    output$plot <- renderPlotly({

      fig <- chart_data() %>% mutate(State = fct_reorder(State, `MCV 1 MCV 2 Droupout`, .desc = TRUE)) %>%
        plot_ly(type = "bar", x =~State, y=~`MCV 1 MCV 2 Droupout`,
                color = I("#edb952"),
                hovertemplate = paste('<b>Rate</b>: %{y:.1f}',
                                      '<br><b style="text-align:left;">State </b>: %{x}<br>',
                                      "<extra></extra>"))%>%
        layout(
          title = paste0("Year: ", picker_year_var()),
          title= list(size=10),
          title = paste(picker_year_var()),
          plot_bgcolor = "rgba(0, 0, 0, 0)",
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          legend = list(orientation = "h",
                        x = 0.4,
                        y = -0.25),
          hoverlabel = list(font = font2),
          font = font,
          # margin = list(r = 80),


          xaxis = list(title = "States",
                       tickfont = font,
                       title= font_axis_title,
                       ticks = "outside",
                       #fixedrange = TRUE,
                       showline = T,
                       title = list(size = 12), tickangle=-45, tickfont = list(size = 12)),
          yaxis = list(range = c(0, 100),title = 'MCV 1 MCV 2 Droupout Rate(%)',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                       title = font_axis_title, tickfont = font)) %>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 8- MCV1 and MCV2 Drop Out Rate.png"))

      fig



    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 8- MCV1 and MCV2 Drop Out Rate.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )

  })
}

## To be copied in the UI
# mod_mcv1_mcv2_drop_out_rate_ui("mcv1_mcv2_drop_out_rate_1")

## To be copied in the server
# mod_mcv1_mcv2_drop_out_rate_server("mcv1_mcv2_drop_out_rate_1")
