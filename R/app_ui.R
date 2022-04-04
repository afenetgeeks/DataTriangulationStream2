#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dbplyr
#' @importFrom shinyMobile f7Page f7SingleLayout f7Row
#' @noRd


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(


      theme =  bslib::bs_theme(version = 5,
                               heading_font = bslib::font_google("Ubuntu"),
                               base_font = bslib::font_google("Roboto")),

        HTML(
          "<html>
      <head>
      <!-- Place your kit's code here -->

      <script src='https://kit.fontawesome.com/e8ca843dba.js' crossorigin='anonymous'></script>

      <meta name='viewport' content='width=device-width, initial-scale=1.0'>

      </head>
    </html>"
        ),

      mod_dashboard_heading_ui("dashboard_heading_1"),

      mod_inputs_ui("inputs_1"),

      div(class = "row-page",
          mod_national_measles_coverage_different_sources_ui("national_measles_coverage_different_sources_1")),

      div(class = "row-page",
      mod_confirmed_measles_cases_MCV1_coverage_ui("confirmed_measles_cases_MCV1_coverage_1"),
      mod_age_group_of_confirmed_measles_cases_by_vaccination_status_ui("age_group_of_confirmed_measles_cases_by_vaccination_status_1")
      ),
      div(class = "row-page",
        mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_ui("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1"),
        mod_measles_vaccine_stock_analysis_measles_coverage_ui("measles_vaccine_stock_analysis_measles_coverage_1")
      ),
      div(class = "row-page",
        mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1"),
        mod_mcv1_mcv2_drop_out_rate_nigeria_ui("mcv1_mcv2_drop_out_rate_nigeria_1")
      ),
      div(class = "row-page",
        mod_mcv1_mcv2_drop_out_rate_ui("mcv1_mcv2_drop_out_rate_1"),
        mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("discrepancy_mcv1_yellow_fever_given_by_state_1")
      ),

      div(class = "row-page",
        mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui("map_confirmed_measles_cases_mcv1_coverage_annual_data_1"),
        mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")
      ),
      mod_footer_information_ui("footer_information_1")

      )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'DataTriangulationStream2'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

