FROM rhub/rig:latest

RUN rig install 4.2.0

RUN rig default 4.2.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libicu-dev pkg-config # for stringi on R 4.2.0

RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/jammy/2023-05-16"))' >> /root/.Rprofile

COPY original /home/r_and_p/original

## save the outputs here
RUN mkdir -p /home/r_and_p/original/outputs

## move the outputs here at container run time
RUN mkdir -p /home/r_and_p/original/shared_folder

RUN R -e "setwd('/home/r_and_p/original');source('install_deps.R')"

## Run the original scripts at build time
RUN R -e "setwd('/home/r_and_p/original');source('multiracial_jop_final2020.R')"

## Now copy the outputs to our disk at container run time
CMD mv /home/r_and_p/original/outputs/* /home/r_and_p/original/shared_folder/ 
