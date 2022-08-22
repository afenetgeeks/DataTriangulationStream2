test_that("Data from database has constant column names", {


  shiny::testServer(mod_confirmed_measles_cases_MCV1_coverage_server,
             args = list(picker_year_var = reactive("2022"),
                         picker_month_var = reactive(months_vector_util()),
                         picker_state_var = reactive(national_util()),
                         picker_lga_var = reactive("State level data")), {

    expect_equal(chart_data() %>% names(),
      #dplyr::tbl(connection, "measles_alt_denominator")%>%head(1) %>% collect() %>% names(),
    c("Measles 1 given","State","LGA","Months","Year","Measles 2 given","MCV 1 Alt Denominator",
      "MCV 2 Alt Denominator", "MCV 1","MCV 2","Measles Cases (CaseBased)"))

  })

})
