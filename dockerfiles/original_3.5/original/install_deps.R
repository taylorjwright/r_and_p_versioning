groundhog_lib <- "/home/r_and_p/groundhog_folder"

groundhog::set.groundhog.folder(groundhog_lib)

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
    "2020-05-15"
    )
