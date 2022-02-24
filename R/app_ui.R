#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


national <- "Nigeria"
states_vec <- "Abia"
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    f7Page(

      preloader = T,
      loading_duration = 5,
      options = list( dark = F, filled = T, color ="#13a7b4", iosTranslucentBars = T, navbar = list(iosCenterTitle = TRUE,hideOnPageScroll = F)),
      title = "Nigeria RI/VPDs Data Triangulation Dashboard",

      f7SingleLayout(
        navbar = NULL,
      mod_inputs_ui("inputs_1"),
      mod_single_plot_row_ui("single_plot_row_1")

      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'DataTriangulationStream2'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

