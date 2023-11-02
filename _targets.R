library(targets)
library(tarchetypes)

list(
  tar_target(
    name = paper,
    quarto::quarto_render("paper/"),
    format = "file"
  )

)
