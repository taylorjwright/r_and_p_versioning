library(targets)
library(tarchetypes)

source("functions/helpers.R")

tar_option_set(packages = c(
                 "dplyr",
                 "haven",
                 "forcats",
                 "janitor",
                 "scales",
                 "skimr",
                 "ggplot2",
                 "ggridges",
                 "questionr",
                 "ZeligChoice"
               )
               )


list(
  tar_target(
    data_path,
    "/home/r_and_p/data/multiracial.sav"
  ),
  tar_target(
    multiracial_raw,
    read_sav(data_path)
  ),
  tar_target(
    multiracial_1,
    basic_clean_1(multiracial_raw)
  ),
  tar_target(
    multiracial_2,
    closeness_2(multiracial_1)
  ),
  tar_target(
    multiracial_3,
    linked_fate_3(multiracial_2)
  ),
  tar_target(
    multiracial_4,
    remove_iat_4(multiracial_3)
  ),
  tar_target(
    multiracial_5,
    party_id_5(multiracial_4)
  ),
  tar_target(
    multiracial_6,
    ideology_6(multiracial_5)
  ),
  tar_target(
    multiracial_7,
    police_7(multiracial_6)
  ),
  tar_target(
    multiracial_8,
    cultural_8(multiracial_7)
  ),
  tar_target(
    multiracial,
    social_9(multiracial_8)
  ),
  tar_target(
    race_table,
    count(multiracial, race)
  ),
  tar_target(
    educ_table,
    count(multiracial, educ3, educ)
  ),
  tar_target(
    age_stats,
    skim(multiracial, age)
  ),
  tar_target(
    density_age,
    {ggplot(data=multiracial) +
       geom_density_ridges(aes(age, race, fill = race),
                           alpha = .5)
       }
  ),
  tar_target(
    relig_table,
    count(multiracial, salRelig, q6f)
  ),
  tar_target(
    income_table,
    count(multiracial, income)),
  tar_target(
    southern_table,
    arrange(count(multiracial, inputstate, south), desc(south))
  ),
  tar_target(
  party_id_table,
  count(multiracial, pid, q21)
  ),
  tar_target(
    table_1,
    #identity(1)
    make_table_1(multiracial)
  )
)
