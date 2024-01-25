
#' @rdname measles_page
diphtheria_page <- function() {
  id <- "diphtheria_page"
  href <-  "/diphtheria_page"

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
    </html>"),

                    # loading_screen(),

                    mod_dashboard_heading_ui("dashboard_heading_4"),

                     mod_inputs_ui("inputs_4",disease = disease_list_util()$diphtheria_page),

                    div(class = "row-page",
                        mod_penta_coverage_diphtheria_confirmed_cases_ui("penta_coverage_diphtheria_confirmed_cases_1"),
                        mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_ui("age_group_of_confirmed_diphtheria_cases_by_vaccination_status_1")
                    ),

                    div(class = "row-page",
                        mod_penta_vaccine_stock_analysis_penta_coverage_ui("penta_vaccine_stock_analysis_penta_coverage_1"),
                        mod_discrepancy_penta1_opv1_given_by_state_ui("discrepancy_penta1_opv1_given_by_state_1")
                    ),
                    div(class = "row-page",
                        mod_penta1_penta3_drop_out_rate_nigeria_ui("penta1_penta3_drop_out_rate_nigeria_1"),
                        mod_national_penta_coverage_different_sources_ui("national_penta_coverage_different_sources_1")
                    ),

                    mod_map_confirmed_diphtheria_cases_penta1_coverage_annual_data_ui("map_confirmed_diphtheria_cases_penta1_coverage_annual_data_1"),

                    mod_footer_information_ui("footer_information_1") ),

    server = function(input, output, session) {

      dropdown_inputs <- mod_inputs_server("inputs_4")

      # mod_penta_coverage_diphtheria_confirmed_cases_server("penta_coverage_diphtheria_confirmed_cases_1"  ,
      #                                        picker_year_var = reactive({dropdown_inputs$picker_year_var}),
      #                                        picker_month_var = reactive({dropdown_inputs$picker_month_var}),
      #                                        picker_state_var = reactive({dropdown_inputs$picker_state_var}),
      #                                        picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
      # )


      # mod_age_group_of_confirmed_diphtheria_cases_by_vaccination_status_server("age_group_of_confirmed_diphtheria_cases_by_vaccination_status_1",
      #                                                                            picker_year_var = reactive({dropdown_inputs$picker_year_var}),
      #                                                                            picker_month_var = reactive({dropdown_inputs$picker_month_var}),
      #                                                                            picker_state_var = reactive({dropdown_inputs$picker_state_var}),
      #                                                                            picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
      # )


      # mod_penta_vaccine_stock_analysis_penta_coverage_server("penta_vaccine_stock_analysis_penta_coverage_1",
      #                                                                      picker_year_var = reactive({dropdown_inputs$picker_year_var}),
      #                                                                      picker_month_var = reactive({dropdown_inputs$picker_month_var}),
      #                                                                      picker_state_var = reactive({dropdown_inputs$picker_state_var}),
      #                                                                      picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
      # )

      #
      #
      # mod_discrepancy_penta1_opv1_given_by_state_server("discrepancy_penta1_opv1_given_by_state_1",
      #                                                         picker_year_var = reactive({dropdown_inputs$picker_year_var}),
      #                                                         picker_month_var = reactive({dropdown_inputs$picker_month_var}),
      #                                                         picker_state_var = reactive({dropdown_inputs$picker_state_var}),
      #                                                         picker_lga_var   = reactive({dropdown_inputs$picker_lga_var}))

      mod_penta1_penta3_drop_out_rate_nigeria_server("penta1_penta3_drop_out_rate_nigeria_1",
                                                     picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                     picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                     picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                     picker_lga_var   = reactive({dropdown_inputs$picker_lga_var}))

      mod_national_penta_coverage_different_sources_server("national_penta_coverage_different_sources_1")
       #
      # mod_map_confirmed_diphtheria_cases_diphtheria_coverage_annual_data_server("map_confirmed_diphtheria_cases_diphtheria_coverage_annual_data_1")
    }
  )
}

# Add this to the brochureApp call in R/diphtheria_page run_app.R
# diphtheria_page()
