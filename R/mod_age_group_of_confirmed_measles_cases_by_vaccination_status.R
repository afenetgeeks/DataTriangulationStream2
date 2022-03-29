#' age_group_of_confirmed_measles_cases_by_vaccination_status UI Function
#'
#' @description A shiny Module. slide 4
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyMobile f7Shadow f7Col f7Card f7DownloadButton
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_age_group_of_confirmed_measles_cases_by_vaccination_status_ui <- function(id){
  ns <- NS(id)
  tagList(

    f7Col(
      f7Shadow(
        intensity = 4,
        hover = TRUE,
        f7Card(
          title = NULL,
          splitLayout(h4("Chart 3: Age Group of Confirmed Measles Cases by Vaccination Status",align = "center"),
                      f7DownloadButton(ns("download_chart_data"),label = NULL),
                      cellWidths = c("95%", "5%")),
          withSpinner(plotlyOutput(ns("plot")),type = 6, size = 0.3,hide.ui = F)
        ) )
    )

  )
}

#' age_group_of_confirmed_measles_cases_by_vaccination_status Server Functions
#' @importFrom plotly renderPlotly plot_ly  add_trace layout config
#' @noRd
mod_age_group_of_confirmed_measles_cases_by_vaccination_status_server <-  function(id,
                                                                                   picker_year_var,
                                                                                   picker_month_var,
                                                                                   picker_state_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    chart_data <- reactive({ mvc_by_age_group %>%
      filter(Year %in% picker_year_var() &
               State %in% picker_state_var() &
               Months %in% picker_month_var()) %>%
      group_by(`Age group`) %>%
      summarise(across(c(Vaccinated,Unvaccinated,Unknown), ~ sum(.x, na.rm = TRUE))) %>%
      ungroup()
      })

    output$plot <- renderPlotly({
      p4 <- plot_ly(chart_data(),
                    x = ~`Age group`,
                    y = ~Unknown,
                    hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                          '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),
                    type = "bar",
                    color =  I("#004e64"),
                    name = "Unknown") %>%
        add_trace(y = ~Unvaccinated,
                  color = I("#00a5cf"),
                  hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                        '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),

                  name = "Unvaccinated") %>%
        add_trace(y = ~Vaccinated,
                  hovertemplate = paste('<b>Cases</b>: %{y:.0f}',
                                        '<br><b style="text-align:left;">Age group</b>: %{x}<br>'),

                  color = I("#edb952"),
                  name = "Vaccinated") %>%
        layout(title = paste(paste0("State: ", picker_state_var()),paste0("Year: ", picker_year_var()), sep = "     "),
               title= list(size=10),
               barmode = 'stack',
               xaxis = list(tickfont = font,
                            title = "Age group (M- months)",
                            #fixedrange = TRUE,
                            title= font_axis_title,
                            ticks = "outside",
                            showline = TRUE
               ),

               #width = "auto",
               # autosize = F,

               plot_bgcolor = "rgba(0, 0, 0, 0)",
               paper_bgcolor = 'rgba(0, 0, 0, 0)',

               yaxis = list(side = 'left', rangemode="tozero", title = 'Number of cases',showline = TRUE, showgrid = FALSE, zeroline = T, ticks = "outside",
                            title = font_axis_title, tickfont = font),

               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5,
                             y = -0.25),
               hoverlabel = list(font = font2),
               font = font)%>%config(modeBarButtons = list(list("toImage", "resetScale2d", "zoomIn2d", "zoomOut2d")),
                                     displaylogo = FALSE, toImageButtonOptions = list(filename = "Chart 3- Age Group of Confirmed Measles Cases by Vaccination Status.png"))


      p4

    })

    output$download_chart_data <- downloadHandler(
      filename = "Chart 3- Age Group of Confirmed Measles Cases by Vaccination Status.csv",
      content = function(file) {
        readr::write_csv(chart_data(), file)
      }
    )


  })
}

## To be copied in the UI
# mod_age_group_of_confirmed_measles_cases_by_vaccination_status_ui("age_group_of_confirmed_measles_cases_by_vaccination_status_1")

## To be copied in the server
# mod_age_group_of_confirmed_measles_cases_by_vaccination_status_server("age_group_of_confirmed_measles_cases_by_vaccination_status_1")
