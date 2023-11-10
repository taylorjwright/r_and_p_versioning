FROM rhub/rig@sha256:0667321839be0c2ade5c14fd8c8bbb311a843ddf690081c5d0dd80dcc98bd6c6

# Install the required version of R
RUN rig install 4.2.0

RUN rig default 4.2.0

# Install system level dependencies
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libicu-dev pkg-config libglpk-dev # for stringi on R 4.2.0

# Set the CRAN repositories to a frozen URL from Posit's PPM
RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/__linux__/jammy/2023-05-16"))' >> /root/.Rprofile

# Create required folder structure
RUN mkdir -p /home/r_and_p/data

## move the outputs here at container run time
RUN mkdir -p /home/r_and_p/shared_folder

# Install the required dependencies for the project
COPY install_deps.R /home/r_and_p/install_deps.R
RUN R -e "setwd('/home/r_and_p/');source('install_deps.R')"

# Install Zelig's and ZeligChoice's dependencies
RUN R -e "install.packages(c('sandwich', 'VGAM', 'jsonlite', 'AER', 'plyr', 'dplyr', 'quantreg', 'geepack', 'MCMCpack', 'maxLik', 'Amelia', 'MatchIt', 'survey'))"

# Install Zelig from the CRAN archives, since it's not available anymore
RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.6.tar.gz', repos=NULL, type='source')"

# Install ZeligChoice from the CRAN archives, since it's not available anymore
RUN R -e "install.packages('http://cran.r-project.org/src/contrib/Archive/ZeligChoice/ZeligChoice_0.9-6.tar.gz', repos=NULL, type='source')"

# Copy scripts and folders to the right spot inside the image
COPY functions /home/r_and_p/functions
COPY _targets.R /home/r_and_p/_targets.R
COPY data/raw/dataverse_files/multiracial.sav /home/r_and_p/data/multiracial.sav

# Create the _targets directory because I'll be saving the DAG
# before running it, so the directory won't exist at that point
RUN mkdir /home/r_and_p/_targets

## Get the network before running
RUN R -e "setwd('/home/r_and_p/');htmltools::save_html(html = targets::tar_visnetwork(), file = '/home/r_and_p/_targets/dag_before.html')"

## Run the targets pipeline at image build time
RUN R -e "setwd('/home/r_and_p/');targets::tar_make()"

## Get the network after running
RUN R -e "setwd('/home/r_and_p/');htmltools::save_html(html = targets::tar_visnetwork(), file = '/home/r_and_p/_targets/dag_after.html')"

## Change the line 337 in helpers.R which will change the function oneto0
RUN sed -i '337s/.*/as.numeric(x) %>% scales::rescale(c(1, 0))/' /home/r_and_p/functions/helpers.R

## The network is now out-of-date
RUN R -e "setwd('/home/r_and_p/');htmltools::save_html(html = targets::tar_visnetwork(), file = '/home/r_and_p/_targets/dag_ood.html')"

## Now copy the outputs to our disk at container run time
CMD mv /home/r_and_p/_targets/* /home/r_and_p/shared_folder/ 
