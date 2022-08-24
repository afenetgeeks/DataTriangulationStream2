#' Create the disease filter UI
#'
#' @param disease a string of a disease one of "Measles" or "Yellow Fever"or "Meningitis"
#'
#' @return a shiny.tag of the disease filter
#'

nav_links <- function(disease){

  stopifnot(disease %in% c("Measles","Yellow Fever","Meningitis"))

  tags$ul(id="nav",

          tags$li( tags$a(disease),

                   tags$ul(

                     tags$li(
                       tags$a(href = "/DataTriangulationStream2-disease-filter/", "Measles"),
                     ),
                     tags$li(
                       tags$a(href = "/DataTriangulationStream2-disease-filter/yellow_fever_page", "Yellow Fever"),
                     ),
                     tags$li(
                       tags$a(href = "/DataTriangulationStream2-disease-filter/meningitis_page", "Meningitis"),
                     )
                   )
          )
  )
}


# nav_links <- function(disease){
#
#
#   tags$ul(id="nav",
#
#           tags$li( tags$a(disease),
#
#                    tags$ul(
#
#                      tags$li(
#                        tags$a(href = "/measles_page", "Measles"),
#                      ),
#                      tags$li(
#                        tags$a(href = "/yellow_fever_page", "Yellow Fever"),
#                      ),
#                      tags$li(
#                        tags$a(href = "/meningitis_page", "Meningitis"),
#                      )
#                    )
#           )
#   )
# }


#' Create the loading screen
#'
#' @returns "a List"
#'
#' @description The loading with

loading_screen  <-  function(){

  waiter::waiterPreloader(html =  tags$div(waiter::spin_loaders(42, color = "#008686"),
                                   tags$p("Loading ...", style = "color:gray;"),
                                   tags$p('Hausa: Sannu da zuwa \U0001f44b\U0001f3ff ..., Yoruba: E kaab\u1ecd \U0001f64b\U0001f3ff\u200d\u2640 ...,Igbo: Nn\u1ecd\u1ecd\U0001f1f3\U0001f1ec'),

  ),
  color = "#eff3f4",
  fadeout = 100)
}




#' Create the chart label
#'
#' @returns a character string
#'
#' @description Create the chart label which shows the State:LGA text in the
#' place of the chart title.


chart_label <- function(picker_state_var,picker_lga_var ){
  if(picker_state_var =="Federal Government"){

    paste(picker_state_var)

  }else{

    paste( picker_state_var , "," ,picker_lga_var)
  }
}


#' Style font of the plot title

font_plot_title <- function(){
  list(
    family = "Roboto, sans-serif",
    color = "black",
    size = 11)
}

#' Style font  of the plot text in the plotting area
font_plot <-  function(){
  list(
    family = "Roboto, sans-serif",
    color = "black",
    size = 10)
}

#' Style font  of the plot text used in the axises
font_axis_title <-  function(){
  list(
    family = "Roboto, sans-serif",
    color = "black",
    size = 10)
}


#' Style font  of the text used when a user hovers over a plot

font_hoverlabel <- function(){
  list(
    family = "Roboto, sans-serif",
    color = "white",
    size = 10)
}


measles_plot_bgcolor  <- function(){
  "rgba(0, 0, 0, 0)"

}

measles_paper_bgcolor <- function(){
  "rgba(0, 0, 0, 0)"
}


yf_plot_bgcolor <-  function(){
  "rgba(0, 0, 0, 0)"
}


yf_paper_bgcolor <-  function(){
  "rgba(0, 0, 0, 0)"
}

plot_margin <- function(){
  list(r = 83, l = 81)
}

plot_margin_one_side <- function(){
  list(r = 81, l = 81)
}

#' Determine the scale range of percentage y-axis
#'
#' @description
#' Determine the percentage y-axis where if its beyond 100% let the upper limit be
#' the maximum point in the data plus 50% of that maximum point
#'
#' @details
#'
#' formula used
#'
#' if(max_rate  > 100){  max_rate + (max_rate/ 2) }
#'
#' Same rule applies to other min_rate but towards the negative side

plot_rate_range <- function(min_rate, max_rate){


  if(max_rate < 101 ){

    if(min_rate > 0  ){
      c(0, 100)

    }else{

      c(min_rate + (min_rate/2), 100)
    }

  }else{

    if(  min_rate > 0 ){

      c(0,  max_rate + (max_rate/ 2))

    }else{

      c( min_rate + (min_rate/2) , max_rate + (max_rate/ 2) )
    }

  }


}


#' Determine the scale range of number y-axis
#'
#' @description
#' Determine the number y-axis where if its beyond 100% let the upper limit be
#' the maximum point in the data plus 50% of that maximum point
#'
#' @details
#'
#' formula used
#'
#' if(max_number > 100){  max_number+ (max_number/ 2) }
#'
#' Same rule applies to other min_number but towards the negative side



plot_number_range <-  function( min_number, max_number){


  if(min_number > 0){

    c(0,  max_number + (max_number/ 2))

  }
  else{
    c(min_number + ( min_number/2) , max_number + (max_number/ 2) )
  }

}


#' Create a list of diseases
#'
#' @description
#' Create a list of diseases in the Dashboard in one place. So that you keep track of the spellings and
#'  easy additional extra diseases in the future

disease_list_util <-  function(){

  list(measles_page= "Measles",
       yellow_fever_page =  "Yellow Fever",
       meningitis_page = "Meningitis")
}


#' Create National level selected vector

national_util <- function(){
  states$state_name[38]
}

#' Create the states choices
#'
states_vector_util <-  function(){
  states$state_name[1:37]
}

#' Create the months choices

months_vector_util <- function(){
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
}

#' Create the year choices
#'
years_vector_util <- function(){

  c("2017", "2018", "2019", "2020", "2021", "2022")
}


