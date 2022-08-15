

test_that("Disease List is still constant", {

  expect_type(disease_list_util(), "list")

  expect_equal(c(disease_list_util()$measles_page,disease_list_util()$yellow_fever_page, disease_list_util()$meningitis_page ),
         c("Measles","Yellow Fever","Meningitis"))
})

test_that("Test if the selection vectors are still the same",{

  expect_equal(national_util(), "Federal Government")

  expect_equal(sort(states_vector_util()),
               c("Abia","Adamawa","Akwa-Ibom","Anambra","Bauchi","Bayelsa","Benue","Borno","Cross River","Delta","Ebonyi",
                 "Edo","Ekiti","Enugu","Federal Capital Territory","Gombe","Imo","Jigawa","Kaduna","Kano","Katsina","Kebbi",
                 "Kogi","Kwara","Lagos","Nasarawa","Niger","Ogun","Ondo","Osun","Oyo","Plateau","Rivers","Sokoto","Taraba","Yobe","Zamfara"))

  expect_equal(months_vector_util(),  c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

})


test_that("Disease selected is the right spelling", {

  golem::expect_shinytag(nav_links(disease_list_util()$measles_page))

  golem::expect_shinytag(nav_links(disease_list_util()$yellow_fever_page))

  golem::expect_shinytag(nav_links(disease_list_util()$meningitis_page))

  ## Just in class the spelling is wrong
   expect_error(nav_links("Measesl"))


})

test_that("Is Chart label still consistent ", {

 expect_equal(chart_label(picker_state_var = "Federal Government", picker_lga_var = "State level data"),
              "Federal Government")

  expect_equal(chart_label(picker_state_var = "Abuja", picker_lga_var = "State level data"),
               "Abuja , State level data")

})


test_that("National level seletor is Federal Government",{
          expect_equal(national_util(),  "Federal Government" )
          })






