#' mcv1_mcv2_drop_out_rate UI Function
#'
#' @description A shiny Module slide 9 -----
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_mcv1_mcv2_drop_out_rate_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t",
        div(class ="column-icon-div",
            img(class = "column-icon", src = "www/total-registrations-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        h6("Chart 8: Measles 1 and Measles 2 Drop Out Rate", class = "column-title"),
        HTML('<a id="downloadData" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>                       <i class="fa fa-download" aria-hidden="true"></i>                       <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>                      </a>'),          HTML('<a id="downloadChart" class="btn btn-default shiny-download-link download-data-btn download-chart-btn" href="" target="_blank" download>                      <i class="fa fa-chart-bar"></i>                       <div class = tooltipdiv>                           <p class="tooltiptext">                               Download this Chart                           </p>                       </div>                      </a>'),
        withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)

    )



  )
}

#' mcv1_mcv2_drop_out_rate Server Functions
#' @importFrom forcats fct_reorder
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#' @importFrom dplyr collect tbl mutate arrange filter across
#' @noRd
mod_mcv1_mcv2_drop_out_rate_server <- function(id,
                                               picker_year_var,
                                               picker_month_var,
                                               picker_state_var
                                               ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    chart_data <- reactive({
      # slide 9
      dplyr::tbl(stream2_pool, "s9_combined")%>%
        filter(Year %in% !!picker_year_var() &
                 Months %in%  !!picker_month_var())%>%dplyr::collect()%>%
        dplyr::mutate(dplyr::across(.col = c(Year,State, Months ), as.factor))


    })

    output$plot <- renderPlotly({

      fig <- chart_data() %>% mutate(State = fct_reorder(State, `MCV 1 MCV 2 Droupout`, .desc = TRUE)) %>%
        plot_ly(type = "bar", x =~State, y=~`MCV 1 MCV 2 Droupout`,
                color = I("#edb952"),
                hovertemplate = paste('<b>Rate</b>: %{y:.1f}',
                                      '<br><b style="text-align:left;">State </b>: %{x}<br>',
                                      "<extra></extra>"))%>%
        layout(
          title = paste0("Year: ", picker_year_var()),
         # title= list(size=10),
          title = paste(picker_year_var()),
          plot_bgcolor = "rgba(0, 0, 0, 0)",
          paper_bgcolor = 'rgba(0, 0, 0, 0)',
          legend = list(orientation = "h",
                        x = 0.4,
                        y = -0.25),
          hoverlabel = list(font = font_hoverlabel()),
          font = font_plot(),
          # margin = list(r = 80),


          xaxis = list(title = "States",
                       tickfont = font_plot(),
                       title= font_axis_title(),
                       ticks = "outside",
                       #fixedrange = TRUE,
                       showline = T,
                       tickangle=-45
                       ),
          yaxis = list(range = c(0, 100),title = 'MCV 1 MCV 2 Droupout Rate(%)',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                       title = font_axis_title(), tickfont = font_plot())) %>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 8- Measles 1 and Measles 2 Drop Out Rate.png"))

      fig



    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 8- Measles 1 and Measles 2 Drop Out Rate.csv",
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
