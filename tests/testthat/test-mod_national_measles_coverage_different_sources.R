test_that("Data from database has constant column names", {
  testServer(mod_national_measles_coverage_different_sources_server, {

                           expect_equal(chart_data() %>% names(),
                                         #dplyr::tbl(connection, "mcv_different_sources")%>%head(1) %>% collect() %>% names(),
                                        c("Year","PMCCS","WUENIC (MCV1)","WUENIC (MCV2)","NDHS","SMART Survey", "NICS/MICS",
                                          "Dhis2 (MCV1)", "Dhis2 (MCV2)" ))

                         })

})
