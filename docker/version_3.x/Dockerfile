FROM rhub/rig@sha256:0667321839be0c2ade5c14fd8c8bbb311a843ddf690081c5d0dd80dcc98bd6c6

RUN rig install 3.5.0

RUN rig default 3.5.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev

RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/jammy/2019-05-16"))' >> /root/.Rprofile

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
