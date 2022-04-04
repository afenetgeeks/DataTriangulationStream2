#' age_group_of_confirmed_yellow_fever_cases_by_vaccination_status UI Function
#'
#' @description A shiny Module for slide 5 for chart 4 in on the dashboard
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "col-6 col-6-t",
        div(class ="column-icon-div",
            img(class = "column-icon", src = "www/age-group-vaccination-icon.svg",  height = 40, width = 80, alt="nigeria coat of arms", role="img")),

        h6("Chart 4: Age Group of Confirmed Yellow Fever Cases by Vaccination Status", class = "column-title"),
        HTML('<a id="downloadData" class="btn btn-default shiny-download-link download-data-btn" href="" target="_blank" download>                       <i class="fa fa-download" aria-hidden="true"></i>                       <div class = tooltipdiv> <p class="tooltiptext">Download the data for this Chart</p> </div>                      </a>'),          HTML('<a id="downloadChart" class="btn btn-default shiny-download-link download-data-btn download-chart-btn" href="" target="_blank" download>                      <i class="fa fa-chart-bar"></i>                       <div class = tooltipdiv>                           <p class="tooltiptext">                               Download this Chart                           </p>                       </div>                      </a>'),
        withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)

    )
  )
}

#' age_group_of_confirmed_yellow_fever_cases_by_vaccination_status Server Functions
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param picker_year_var,picker_month_var,picker_state_var Selected parameters from the inputs
#'
#' @noRd
mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_server <- function(id,
                                                                                       picker_year_var,
                                                                                       picker_month_var,
                                                                                       picker_state_var
                                                                                       ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # slide 5
    chart_data <- reactive({

      dplyr::tbl(stream2_pool, "yf_by_age_group")%>%
        filter(Year %in% !!picker_year_var() &
                 State %in% !!picker_state_var() &
                 Months %in%  !!picker_month_var()) %>%group_by(`Age group`) %>%
        summarise(across(c(Vaccinated,Unvaccinated,Unknown), ~ sum(.x, na.rm = TRUE))) %>%  ungroup() %>% dplyr::collect() %>%
        dplyr::mutate(`Age group` = factor(`Age group`, labels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35"),
                                           levels = c("0-8 M", "9-23 M", "24-48 M", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", ">=35")))

      })


    output$plot <- renderPlotly({

      p5 <- plot_ly(chart_data(),
                    x = ~`Age group`,
                    y = ~Unknown,
                    color =  I("#004e64"),
                    hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                          '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),

                    type = "bar",
                    name = "Unknown") %>%
        add_trace(y = ~Unvaccinated,
                  color =I("#00a5cf"),
                  hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                        '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),
                  name = "Unvaccinated") %>%
        add_trace(y = ~Vaccinated,
                  color = I("#edb952"),
                  hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                        '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),
                  name = "Vaccinated") %>%
        layout( title = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()), sep = "     "),
                title= list(size=10),
                barmode = 'stack',
                xaxis = list(tickfont = font_plot(),
                             title = "Age group (M- months)",
                             #fixedrange = TRUE,
                             title= font_axis_title(),
                             ticks = "outside",
                             showline = TRUE),
                #width = "auto",
                # autosize = F,

                plot_bgcolor = "rgba(0, 0, 0, 0)",
                paper_bgcolor = 'rgba(0, 0, 0, 0)',
                yaxis = list(side = 'left', rangemode="tozero", title = 'Number of cases',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                             title = font_axis_title(), tickfont = font_plot()),


                legend = list(orientation = "h",   # show entries horizontally
                              xanchor = "center",  # use center of legend as anchor
                              x = 0.5,
                              y = -0.25),
                hoverlabel = list(font = font_hoverlabel()),
                font = font_plot())%>%
        config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
               displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 4- Age Group of Confirmed Yellow Fever Cases by Vaccination Status.png"))


      p5
    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 4- Age Group of Confirmed Yellow Fever Cases by Vaccination Status.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


  })
}

## To be copied in the UI
# mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_ui("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1")

## To be copied in the server
# mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_server("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1")
