test_that("Data from database has constant column names", {


  shiny::testServer(mod_meningitis_coverage_confirmed_cases_server,
             args = list(picker_year_var = reactive("2022"),
                         picker_month_var = reactive(months_vector_util()),
                         picker_state_var = reactive(national_util()),
                         picker_lga_var = reactive("State level data")), {

                           expect_equal(chart_data() %>% names(),
                                        #dplyr::tbl(connection, "men_A_alt_denominator")%>%head(1) %>% collect() %>% names(),
                                        c("Vaccine doses given", "State","LGA", "Months", "Year", "Disease Alt Denominator" ,"Disease Coverage","Disease Cases"))

                         })

})
