# r_and_p_versioning
Repository for paper "Replication materials are great but software versioning still poses a problem for open science"

The paper is rendered at each push to `main`. The rendered paper can be found in the `targets-runs` branch in the `rendered_pdf` folder.

## Reproducing the results from the original paper

The Dockerfiles inside the `docker` folder can be used to build images based on
rhub/rig that makes it easy to install the required R version. The digest
`0667321839be0c2ade5c14fd8c8bbb311a843ddf690081c5d0dd80dcc98bd6c6` is used for
reproducibility purposes. The original script had to be slightly adapted to
output the results of interest that we need to compare between R version 3.5 and
R version 3.6. Results obtained using R version 4.2 are also provided (are
aligned with R 3.6).

To build the images, go to either the `version_3.x` (for R version 3.5 or 3.6) or go to `version_4.2`
(for R version 4.2) and run the following command:

```
docker build -t version3x .
```

You can name the image whatever you want, so change `version3x` to what you prefer. Once the image is built,
run a container with:

```
docker run -d -it --rm --name original35_container -v /home/path/to/docker/version_3.x/original/shared_folder:/home/r_and_p/original/shared_folder:rw version3x
```

Change the path `/home/path/to/docker/version_3.x/original/shared_folder` to the
one on your machine. The container will run for a few seconds, and you should
see the outputs (two `.rds` files) in the `shared_folder` folder.

## targets port

The `targets_port` folder contains the port of the code to a `{targets}` pipeline.

The file `Dockerfile` contains the instructions to build a Docker image that:

- builds upon the [rig Docker image](https://github.com/r-lib/rig#id-container) by the [*R Infrastructure*](https://github.com/r-lib) team, with the same digest as above
- installs R version 3.5
- installs the R packages required for the original study as of May 16th 2019 using Posit's Public Package Manager
- computes some of the outputs and table 1 from the original study using a `{targets}` pipeline. It then save a visual representation of the pipeline, then one function gets edited to make the pipeline outdated. A new visual representation gets saved, the pipeline gets run again, and then one last representation gets saved. These images are then used in the paper.

Build the image using:

```
docker build --progress=plain -t target_version .
```

Run the container using (change `/path/in/your/pc` to the right path in your pc):

```
docker run -d -it --rm --name target_version_container -v /path/in/your/pc/r_and_p_versioning/shared_folder:/home/r_and_p/shared_folder:rw target_version

```
