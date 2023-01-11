
test_that("Data from database has constant column names", {
  shiny::testServer(mod_map_confirmed_measles_cases_mcv1_coverage_annual_data_server, {

    session$setInputs(picker_state = national_util(),
                      picker_year =  "2022",
                      picker_month = "Year Data")

    expect_equal( names(stream2_data$dhis2_data),
                  # dplyr::tbl(connection, "measles_coverage_map")%>%head(1) %>% collect() %>% names(),
                  c("Year", "Coverage", "State", "LGA", "Months", "Coverage %"))


    expect_equal( names(stream2_data$sormas_mvc),
                  # dplyr::tbl(connection, "measles_cases_map")%>%head(1) %>% collect() %>% names(),
                  c("##Case ID", "Disease", "Long", "Lat", "Months", "Year", "State", "LGA" ))

    })

})
