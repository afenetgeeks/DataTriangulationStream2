#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom RMariaDB MariaDB
#' @importFrom dplyr collect tbl mutate arrange filter across ungroup
#' @importFrom readr read_csv
#' @importFrom GADMTools gadm_sp_loadCountries
#' @importFrom stringr str_c
#' @importFrom magrittr %>%
#'
#' @noRd
#'
webshot::install_phantomjs(version = "2.1.1", force = FALSE)

app_server <- function( input, output, session ) {

  #stream2_pool_connection <- reactive({connect_sql_db()})
  # Your application server logic


  shiny::onStop(function() {
    pool::poolClose(stream2_pool)
  })


  dropdown_inputs <- mod_inputs_server("inputs_1")

  mod_national_measles_coverage_different_sources_server("national_measles_coverage_different_sources_1")

  mod_confirmed_measles_cases_MCV1_coverage_server("confirmed_measles_cases_MCV1_coverage_1",
                                                   picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                   picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                   picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                                  )

  mod_age_group_of_confirmed_measles_cases_by_vaccination_status_server("age_group_of_confirmed_measles_cases_by_vaccination_status_1",
                                                                        picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                        picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                        picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                                                        )

  mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_server("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1",
                                                                             picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                             picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                             picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                                                            )


  mod_measles_vaccine_stock_analysis_measles_coverage_server("measles_vaccine_stock_analysis_measles_coverage_1",
                                                             picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                             picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                             picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                                             )


  mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1",
                                                                       picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                       picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                       picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                                                       )

  mod_mcv1_mcv2_drop_out_rate_nigeria_server("mcv1_mcv2_drop_out_rate_nigeria_1",
                                             picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                             picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                             picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                             )

  mod_mcv1_mcv2_drop_out_rate_server("mcv1_mcv2_drop_out_rate_1",
                                     picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                     picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                     picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                     )

  mod_discrepancy_mcv1_yellow_fever_given_by_state_server("discrepancy_mcv1_yellow_fever_given_by_state_1",
                                                          picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                          picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                          picker_state_var = reactive({dropdown_inputs$picker_state_var})
                                                          )


  mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server("map_confirmed_measles_cases_mcv1_coverage_annual_data_1")

    mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_server("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")
}
