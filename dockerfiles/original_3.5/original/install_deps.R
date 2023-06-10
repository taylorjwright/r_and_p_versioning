# Old versions of Zelig and ZeligChoice
# Actually the below is likely not needed, groundhog should be able to get the required versions
#install.packages('http://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.6.tar.gz',
#                 repos=NULL,
#                 type='source',
#                 lib = groundhog_lib)
#
#install.packages('http://cran.r-project.org/src/contrib/Archive/ZeligChoice/ZeligChoice_0.9-6.tar.gz',
#                 repos=NULL,
#                 type='source',
#                 lib = groundhog_lib)

# the paper was published in may 2020, so set the date accordingly below
# Also, the package library used was likely older than may 2020: this is because
# in may 2020 {foreign} was on version 0.8-79, but that version already required
# R version 4.0, and so cannot be installed on R 3.5.0.
# However, R version 3.5.0 was not the current version at the time, so we need
# to set `tolerate.R.version`

# For some reason, the version of matching as of 2019-12-15 has not been archived?
# It should be version 'Matching_4.9-6', but it's nowhere to be found
# So groundhog cannot install it.
# Let's try install to install the right version from github
remotes::install_github("JasjeetSekhon/Matching",
                        ref = "529d1ad", # commit with version 4.9-6
                        lib = "root/R_groundhog")

groundhog::groundhog.library("
    library(MASS)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(purrr)
    library(broom)
    library(foreign)
    library(Zelig)
    library(ZeligChoice)
    library(nnet)
    library(car)
    library(texreg)
    library(extrafont)
    library(caret)
    library(e1071)
    library(scales)
    library(xtable)
    library(questionr)
    library(ggridges)
    library(reporttools)
    library(stringr)",
    "2019-12-15",
    tolerate.R.version='3.5.0'
    )
