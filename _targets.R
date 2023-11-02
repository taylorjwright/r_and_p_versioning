library(targets)
library(tarchetypes)

list(
  tar_force(
    name = paper,
    command = quarto::quarto_render("paper/"),
    format = "file", 
    force = TRUE
  )

)
