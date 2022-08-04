test_that("Data from database has constant column names", {
  testServer(mod_age_group_of_confirmed_measles_cases_by_vaccination_status_server,
             args = list(picker_year_var = reactive("2022"),
                         picker_month_var = reactive(months_vector_util()),
                         picker_state_var = reactive(national_util()),
                         picker_lga_var = reactive("State level data")), {

                           expect_equal(chart_data() %>% names(),
                                       # dplyr::tbl(connection, "measles_age_group")%>%head(1) %>% collect() %>% names(),
                                        c("Age group","Vaccinated","Unvaccinated","Unknown" ))

                         })

})
