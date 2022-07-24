
#' Page Functions
#'
#' @noRd
#' @importFrom brochure page
#'
#' @import shiny
#' @importFrom RMariaDB MariaDB
#' @importFrom dplyr collect tbl mutate arrange filter across ungroup
#' @importFrom readr read_csv
#' @importFrom GADMTools gadm_sp_loadCountries
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#'
#'
measles_page <- function(id = "measles_page", href = "/") {
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

        mod_dashboard_heading_ui("dashboard_heading_ui_1"),

        mod_inputs_ui("inputs_1",disease = "Measles"),

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
        # mod_home_ui(id = id),
        # nav_links,
        # mod_simple_plot_ui(id)

    server = function(input, output, session) {

       dropdown_inputs <- mod_inputs_server("inputs_1")

      mod_confirmed_measles_cases_MCV1_coverage_server("confirmed_measles_cases_MCV1_coverage_1",
                                                       picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                       picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                       picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                       picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                      )

        mod_age_group_of_confirmed_measles_cases_by_vaccination_status_server("age_group_of_confirmed_measles_cases_by_vaccination_status_1",
                                                                              picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                              picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                              picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                                              picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                                              )

        mod_measles_vaccine_stock_analysis_measles_coverage_server("measles_vaccine_stock_analysis_measles_coverage_1",
                                                                   picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                   picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                   picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                                   picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                                   )

        mod_mcv1_mcv2_drop_out_rate_nigeria_server("mcv1_mcv2_drop_out_rate_nigeria_1",
                                                   picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                   picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                   picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                   picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                   )

            mod_discrepancy_mcv1_yellow_fever_given_by_state_server("discrepancy_mcv1_yellow_fever_given_by_state_1",
                                                                    picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                    picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                    picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                                    picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
            )

      mod_national_measles_coverage_different_sources_server("national_measles_coverage_different_sources_1")

     mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")


    }
  )
}

# Add this to the brochureApp call in R/measles_pagerun_app.R
# measles_page()
