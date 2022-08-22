test_that("Test if the inputs filtering is working", {

  shiny::testServer(mod_inputs_server,{

    session$setInputs(picker_state = "Federal Government")
    session$flushReact()

    expect_equal(lga_list(),"State level data" )
    expect_length(lga_list(), 1 )


    ####

    session$setInputs(picker_state = "Abia")
    session$flushReact()

  #  expect_no_match(lga_list(),"State level data" )

    expect_true(length(lga_list()) > 1 )

  })

})

