
#' Create a Measles/ Yellow Fever/ Meningitis page
#'
#' @description
#'
#'
#' `measles_page()` creates the measles page
#'
#' `yellow_fever_page()` creates the yellow fever page
#'
#' `meningitis_page()` creates the meningitis page
#'
#' `diphtheria_page()` creates the diphtheria page
#'
#' @details
#' Since the output is a Brochure page that means the ui and server function are defined in the page
#'  and hence contains code for the UI and server of the page
#'
#' @returns A Brochure Page

#'


measles_page <- function() {

  id <- "measles_page"

  href <- "/"


  page(

    href = href,


    ui =  fluidPage( theme =  bslib::bs_theme(version = 4,
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

       #   loading_screen(),

        mod_dashboard_heading_ui("dashboard_heading_ui_1"),

        mod_inputs_ui("inputs_1",disease = disease_list_util()$measles_page),

        div(class = "row-page",
            mod_confirmed_measles_cases_MCV1_coverage_ui("confirmed_measles_cases_MCV1_coverage_1"),
            mod_age_group_of_confirmed_measles_cases_by_vaccination_status_ui("age_group_of_confirmed_measles_cases_by_vaccination_status_1")
        ),

        div(class = "row-page",
            mod_measles_vaccine_stock_analysis_measles_coverage_ui("measles_vaccine_stock_analysis_measles_coverage_1"),
            mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("discrepancy_mcv1_yellow_fever_given_by_state_1")
        ),


        div(class = "row-page",
            mod_mcv1_mcv2_drop_out_rate_nigeria_ui("mcv1_mcv2_drop_out_rate_nigeria_1"),
            mod_national_measles_coverage_different_sources_ui("national_measles_coverage_different_sources_1")

        ),
        div(class = "row-page",
            mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_ui("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")
        ),


        mod_footer_information_ui("footer_information_1")

        ),

    server = function(input, output, session) {

      waiter::waiter_hide()

      dropdown_inputs <- mod_inputs_server("inputs_1")
#
#       mod_confirmed_measles_cases_MCV1_coverage_server("confirmed_measles_cases_MCV1_coverage_1",
#                                                        picker_year_var = reactive({dropdown_inputs$picker_year_var}),
#                                                        picker_month_var = reactive({dropdown_inputs$picker_month_var}),
#                                                        picker_state_var = reactive({dropdown_inputs$picker_state_var}),
#                                                        picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
#                                                       )
#
#    mod_age_group_of_confirmed_measles_cases_by_vaccination_status_server("age_group_of_confirmed_measles_cases_by_vaccination_status_1",
#                                                                          picker_year_var = reactive({dropdown_inputs$picker_year_var}),
#                                                                          picker_month_var = reactive({dropdown_inputs$picker_month_var}),
#                                                                          picker_state_var = reactive({dropdown_inputs$picker_state_var}),
#                                                                          picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
#                                                                          )
#
#    mod_measles_vaccine_stock_analysis_measles_coverage_server("measles_vaccine_stock_analysis_measles_coverage_1",
#                                                               picker_year_var = reactive({dropdown_inputs$picker_year_var}),
#                                                               picker_month_var = reactive({dropdown_inputs$picker_month_var}),
#                                                               picker_state_var = reactive({dropdown_inputs$picker_state_var}),
#                                                               picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
#                                                               )
#
#    mod_mcv1_mcv2_drop_out_rate_nigeria_server("mcv1_mcv2_drop_out_rate_nigeria_1",
#                                               picker_year_var = reactive({dropdown_inputs$picker_year_var}),
#                                               picker_month_var = reactive({dropdown_inputs$picker_month_var}),
#                                               picker_state_var = reactive({dropdown_inputs$picker_state_var}),
#                                               picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
#                                               )
#
#        mod_discrepancy_mcv1_yellow_fever_given_by_state_server("discrepancy_mcv1_yellow_fever_given_by_state_1",
#                                                                picker_year_var = reactive({dropdown_inputs$picker_year_var}),
#                                                                picker_month_var = reactive({dropdown_inputs$picker_month_var}),
#                                                                picker_state_var = reactive({dropdown_inputs$picker_state_var}),
#                                                                picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
#        )
#
# mod_national_measles_coverage_different_sources_server("national_measles_coverage_different_sources_1")

#mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")

    }
  )
}

# Add this to the brochureApp call in R/measles_pagerun_app.R
# measles_page()
