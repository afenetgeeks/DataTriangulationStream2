#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @import brochure

# run_app <- function(
#     onStart = NULL,
#     options = list(),
#     enableBookmarking = NULL,
#     ...
# ) {
#   with_golem_options(
#     app = brochureApp(
#       # Putting the resources here
#       golem_add_external_resources(),
#       measles_page(),
#       yellow_fever_page(),
#       meningitis_page(),
#       onStart = onStart,
#       options = options,
#       enableBookmarking = enableBookmarking
#     ),
#     golem_opts = list(...)
#   )
# }


run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    ...
) {
  with_golem_options(
    app = brochureApp(
      # Putting the resources here
      golem_add_external_resources(),
      measles_page(),
      yellow_fever_page(),
      meningitis_page(),
      redirect(
        from = "/measles_page",
        to = "/"
      ),
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      content_404 = "Not found",
      basepath = "",
      req_handlers = list(),
      res_handlers = list(),
      wrapped = shiny::fluidPage
    ),
    golem_opts = list(...)
  )
}

