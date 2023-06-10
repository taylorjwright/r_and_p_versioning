FROM rhub/rig:latest

RUN rig install 3.5.0

RUN rig default 3.5.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev 

RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/jammy/2019-05-16"))' >> /root/.Rprofile

COPY original /home/r_and_p/original

RUN R -e "setwd('/home/r_and_p/original');source('install_deps.R')"

## Run the original scripts at container run time
CMD R -e "setwd('/home/r_and_p/original');source('multiracial_jop_final2020.R')"
