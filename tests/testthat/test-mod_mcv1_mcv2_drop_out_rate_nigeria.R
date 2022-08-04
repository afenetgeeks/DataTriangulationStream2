test_that("Data from database has constant column names", {
  testServer(mod_mcv1_mcv2_drop_out_rate_nigeria_server ,
             args = list(picker_year_var = reactive("2022"),
                         picker_month_var = reactive(months_vector_util()),
                         picker_state_var = reactive(national_util()),
                         picker_lga_var = reactive("State level data")), {

                           expect_equal(chart_data() %>% names(),
                                        # dplyr::tbl(connection, "mcv1_mcv2_dropout_rate")%>%head(1) %>% collect() %>% names(),
                                        c("Months", "Year", "State", "LGA", "first_dose", "second_dose", "dropout_rate"))

                         })

})
