# r_and_p_versioning
Repository for paper "Replication materials are great but software versioning still poses a problem for open science" 

## The 'docker' branch

You can find the `dockerfiles` folder in the `docker` branch. In that folder, you can find several Dockerfiles:

- original3.5: run the original project on R version 3.5.x with package dependencies downloaded as they were on the 15th of May 2020 (using `{groundhog}`)
- original3.6: run the original project on R version 3.6.x with package dependencies downloaded as they were on the 15th of May 2020 (using `{groundhog}`)
- targets3.5: run the project as a targets pipeline on R version 3.5.x with package dependencies downloaded as they were on the 15th of May 2020 (using `{groundhog}`)
- targets3.6: run the project as a targets pipeline on R version 3.6.x with package dependencies downloaded as they were on the 15th of May 2020 (using `{groundhog}`)

The "original" Dockerfiles are needed to rerun the project as it was written at the time, and to illustrate the problems introduced by the change in random
number generation in R 3.6. The "targets" Dockerfiles illustrate how the project can benefit from functional and literate programming (on top of using
`{groundhog}` and Docker) to be completely reproducible.
