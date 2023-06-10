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

# Update: not using groundhog, because:
# for some reason, the version of matching as of 2019-12-15 has not been archived?
# It should be version 'Matching_4.9-6', but it's nowhere to be found
# So groundhog cannot install it. I've tried from Github:
# Let's try install to install the right version from github
#install.packages("remotes")
#
#remotes::install_github("JasjeetSekhon/Matching",
#                        ref = "529d1ad", # commit with version 4.9-6
#                        )
# but then had an issue with the library not being writeable
# The solution I've settled on is using Posit's CRAN snapshots. First, I tried December 
# 2019, but the version of MCMCpack that was available at the time required R 3.6.
# So now, I've settled to May 2019, and now the dependencies get installed.

# I don't think the below is needed
#install.packages(
#  c("sandwich",
#    "VGAM",
#    "jsonlite",
#    "AER",
#    "plyr",
#    "dplyr",
#    "quantreg",
#    "geepack",
#    "MCMCpack",
#    "maxLik",
#    "Amelia",
#    "MatchIt",
#    "survey"))

install.packages(
  c("MASS",
    "dplyr",
    "tidyr",
    "ggplot2",
    "purrr",
    "broom",
    "foreign",
    "Zelig",
    "ZeligChoice",
    "nnet",
    "car",
    "texreg",
    "extrafont",
    "caret",
    "e1071",
    "scales",
    "xtable",
    "questionr",
    "ggridges",
    "reporttools",
    "stringr")
)
