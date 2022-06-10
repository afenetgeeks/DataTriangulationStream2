#' The application User-Interface

webshot::install_phantomjs(version = "2.1.1", force = FALSE)


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @import dbplyr
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

