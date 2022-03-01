#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(

      preloader = T,
      loading_duration = 5,
      options = list( dark = F, filled = T, color ="#13a7b4", iosTranslucentBars = T, navbar = list(iosCenterTitle = TRUE,hideOnPageScroll = F)),
      title = "Nigeria RI/VPDs Data Triangulation Dashboard",

      f7SingleLayout(
        navbar = NULL,

       mod_dashboard_heading_ui("dashboard_heading_1"),

      mod_inputs_ui("inputs_1"),

      f7Row(tags$i(style="color:#0e7290;font-size:10px","Hover over a chart to display the chart download button on the top right corner of that chart")),

      mod_national_measles_coverage_different_sources_ui("national_measles_coverage_different_sources_1"),

      f7Row(
      mod_confirmed_measles_cases_MCV1_coverage_ui("confirmed_measles_cases_MCV1_coverage_1"),
      mod_age_group_of_confirmed_measles_cases_by_vaccination_status_ui("age_group_of_confirmed_measles_cases_by_vaccination_status_1")
      ),
      f7Row(
        mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_ui("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1"),
        mod_measles_vaccine_stock_analysis_measles_coverage_ui("measles_vaccine_stock_analysis_measles_coverage_1")
      ),
      f7Row(
        mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1"),
        mod_mcv1_mcv2_drop_out_rate_nigeria_ui("mcv1_mcv2_drop_out_rate_nigeria_1")
      ),
      f7Row(
        mod_mcv1_mcv2_drop_out_rate_ui("mcv1_mcv2_drop_out_rate_1"),
        mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("discrepancy_mcv1_yellow_fever_given_by_state_1")
      ),

      f7Row(
        mod_inputs_ui("inputs_maps")
      ),

      f7Row(
        mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui("map_confirmed_measles_cases_mcv1_coverage_annual_data_1"),
        mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")
      ),
      mod_footer_information_ui("footer_information_1")

      )
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

