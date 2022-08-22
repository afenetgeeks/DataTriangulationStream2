test_that("Data from database has constant column names", {
  shiny::testServer(mod_discrepancy_mcv1_men_A_server,
             args = list(picker_year_var = reactive("2022"),
                         picker_month_var = reactive(months_vector_util()),
                         picker_state_var = reactive(national_util()),
                         picker_lga_var = reactive("State level data")), {

                           expect_equal(chart_data() %>% names(),
                                        # dplyr::tbl(connection, "measles_men_A_discrepancy")%>%head(1) %>% collect() %>% names(),
                                        c( "Year","State", "Months","LGA","main_vaccine_given", "other_vaccine_given", "discrepancy"))

                         })

})
