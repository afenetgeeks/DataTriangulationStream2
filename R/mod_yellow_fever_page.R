
#' @rdname measles_page
yellow_fever_page <- function() {
  id <- "yellow_fever_page"
  href <-  "/yellow_fever_page"

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

        mod_dashboard_heading_ui("dashboard_heading_2"),

        mod_inputs_ui("inputs_2",disease = disease_list_util()$yellow_fever_page),

        div(class = "row-page",
            mod_yf_coverage_confirmed_cases_ui("yf_coverage_confirmed_cases_1"),
            mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_ui("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1")


        ),

        div(class = "row-page",
            mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_ui("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1"),
            mod_discrepancy_mcv1_yellow_fever_given_by_state_ui("discrepancy_mcv1_yellow_fever_given_by_state_2")
           ),

        mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_ui("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1"),

        mod_footer_information_ui("footer_information_2") ),
        # mod_home_ui(id = id),
        # nav_links,
        # mod_simple_plot_ui(id)

    server = function(input, output, session) {

      dropdown_inputs <- mod_inputs_server("inputs_2")

      mod_yf_coverage_confirmed_cases_server("yf_coverage_confirmed_cases_1"  ,
                                             picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                             picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                             picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                             picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
      )


mod_age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_server("age_group_of_confirmed_yellow_fever_cases_by_vaccination_status_1",
                                                                           picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                           picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                           picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                                           picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
)


mod_yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_server("yellow_fever_vaccine_stock_analysis_yellow_fever_coverage_1",
                                                                     picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                                     picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                                     picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                                     picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                                     )



mod_discrepancy_mcv1_yellow_fever_given_by_state_server("discrepancy_mcv1_yellow_fever_given_by_state_2",
                                                        picker_year_var = reactive({dropdown_inputs$picker_year_var}),
                                                        picker_month_var = reactive({dropdown_inputs$picker_month_var}),
                                                        picker_state_var = reactive({dropdown_inputs$picker_state_var}),
                                                        picker_lga_var   = reactive({dropdown_inputs$picker_lga_var})
                                                        )


       mod_map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_server("map_confirmed_yellow_fever_cases_yellow_fever_coverage_annual_data_1")
    }
  )
}

# Add this to the brochureApp call in R/yellow_fever_pagerun_app.R
# yellow_fever_page()
