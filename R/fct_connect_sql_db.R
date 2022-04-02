#' connect_sql_db
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom RMariaDB MariaDB
#' @importFrom pool dbPool


  dw <- config::get(file = "./inst/app/www/config.yml", "development_stream2")

  stream2_pool <- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    host     = dw$host,
    username = dw$user,
    password = dw$password,
    port     = dw$port,
    dbname   = dw$database,
    minSize = 3,
    maxSize = Inf,    # this could have been omitted since it's the default
    idleTimeout = 3600000  # 1 hour)
  )



  shiny::onStop(function() {
    pool::poolClose(stream2_pool)
  })






