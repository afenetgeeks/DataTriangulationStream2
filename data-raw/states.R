## code to prepare `states` dataset goes here
library(readr)


states <- tibble::tribble(
                            ~state_name,
                               "Rivers",
                                 "Abia",
                              "Zamfara",
                                  "Imo",
                               "Sokoto",
                                "Lagos",
            "Federal Capital Territory",
                                "Kebbi",
                               "Jigawa",
                                "Benue",
                              "Anambra",
                              "Adamawa",
                                "Enugu",
                                "Borno",
                                "Niger",
                                 "Kogi",
                              "Plateau",
                                 "Yobe",
                                  "Edo",
                                "Gombe",
                                 "Kano",
                          "Cross River",
                                 "Ogun",
                             "Nasarawa",
                                 "Osun",
                                "Ekiti",
                                "Kwara",
                               "Taraba",
                                "Delta",
                              "Bayelsa",
                            "Akwa-Ibom",
                                 "Ondo",
                               "Kaduna",
                              "Katsina",
                               "Ebonyi",
                                  "Oyo",
                               "Bauchi",
                   "Federal Government"
            )

readr::write_csv(x = states, "./data-raw/states.csv")


usethis::use_data(states, overwrite = TRUE)



