FROM rhub/rig:latest

RUN rig install 3.5.0

RUN rig default 3.5.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev 


# Repos are set to a fixed MRAN snapshot (see:
# https://github.com/rocker-org/rocker-versioned/blob/1920e7cfc757bad02d041a0bddec1a18b1ebc4c1/r-ver/3.5.0.Dockerfile#L113)
# so set repos to latest to get {groundhog}
#RUN echo "options(repos = c(CRAN='https://packagemanager.rstudio.com/cran/latest'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

# Create required folders
# This folder will hold the entire library
RUN mkdir -p /root/R_groundhog
#
## Install groundhog
RUN R -e "install.packages(c('remotes', 'groundhog'));groundhog::set.groundhog.folder('/root/R_groundhog')"
#
## Copy the required files
COPY original /home/r_and_p/original
#
## Now install dependencies at image build time
RUN R -e "setwd('/home/r_and_p/original');source('install_deps.R')"
#
## Run the original scripts at container run time
CMD R -e "setwd('/home/r_and_p/original');source('multiracial_jop_final2020.R')"
