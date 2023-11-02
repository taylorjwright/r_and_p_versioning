library(targets)
library(tarchetypes)

list(
  tar_quarto(
    name = paper,
    path = "paper/rodrigues_wright_versioning.qmd",
    output_format = "pdf"
  )

)
