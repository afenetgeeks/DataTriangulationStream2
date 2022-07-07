
#' Page Functions
#'
#' @noRd
#' @importFrom brochure page
meningitis_page <- function(id = "meningitis_page", href = "/meningitis_page") {
  page(
    href = href,
    ui = fluidPage( theme =  bslib::bs_theme(version = 4,
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

        mod_dashboard_heading_ui("dashboard_heading_3"),

        mod_inputs_ui("inputs_3", disease = "Meningitis"),

        div(class = "row-page",
            mod_meningitis_coverage_confirmed_cases_ui("meningitis_coverage_confirmed_cases_1"),
            mod_age_group_of_confirmed_meningitis_cases_by_vaccination_status_ui("age_group_of_confirmed_meningitis_cases_by_vaccination_status_1")

        ),

        div(class = "row-page",
          mod_meningitis_vaccine_stock_analysis_ui("meningitis_vaccine_stock_analysis_1"),
        mod_discrepancy_mcv1_men_A_ui("discrepancy_mcv1_men_A_1")

        ),


      mod_map_confirmed_meningitis_cases_coverage_annual_data_ui("map_confirmed_meningitis_cases_coverage_annual_data_1"),



        mod_footer_information_ui("mod_footer_information_3")
        # mod_home_ui(id = id),
        # nav_links,
        # mod_simple_plot_ui(id)
      ),

    server = function(input, output, session) {

      dropdown_inputs <- mod_inputs_server("inputs_3")

      mod_meningitis_coverage_confirmed_cases_server("meningitis_coverage_confirmed_cases_1",
                                                     picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                     picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                     picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                     picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                     )

       mod_age_group_of_confirmed_meningitis_cases_by_vaccination_status_server("age_group_of_confirmed_meningitis_cases_by_vaccination_status_1",
                                                                                picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                                picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                                picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                                                picker_lga_var   = reactive({dropdown_inputs$picker_lga_var}))

       mod_meningitis_vaccine_stock_analysis_server("meningitis_vaccine_stock_analysis_1",
                                                    picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                    picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                    picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                    picker_lga_var   = reactive({dropdown_inputs$picker_lga_var}))

       mod_discrepancy_mcv1_men_A_server("discrepancy_mcv1_men_A_1",
                                         picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                         picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                         picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                         picker_lga_var   = reactive({dropdown_inputs$picker_lga_var}))



     mod_map_confirmed_meningitis_cases_coverage_annual_data_server("map_confirmed_meningitis_cases_coverage_annual_data_1")

    }
  )
}

# Add this to the brochureApp call in R/meningitis_pagerun_app.R
# meningitis_page()
