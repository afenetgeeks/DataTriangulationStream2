
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

        mod_dashboard_heading_ui("mod_dashboard_heading_3"),

        mod_inputs_ui("mod_input_3"),

        div(class = "row-page",
            mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_ui("mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_2"),
            mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui("mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_2")

        ),

        div(class = "row-page",
            mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("mod_discrepancy_mcv1_yellow_fever_given_by_state_3"),
            mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui("mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_2")
        ),

        mod_footer_information_ui("mod_footer_information_3")
        # mod_home_ui(id = id),
        # nav_links,
        # mod_simple_plot_ui(id)
      ),

    server = function(input, output, session) {


     # mod_meningitis_page_server(id = id)
    }
  )
}

# Add this to the brochureApp call in R/meningitis_pagerun_app.R
# meningitis_page()
