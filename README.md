# r_and_p_versioning
Repository for paper "Replication materials are great but software versioning still poses a problem for open science"

## Reproducing the results from the original paper

The Dockerfiles inside the `docker` folder can be used to build images based on rhub/rig that makes
it easy to install the required R version. The original script had to be slightly adapted to output the
results of interest that we need to compare between R version 3.5 and R version 3.6. Results obtained
using R version 4.2 are also provided (are aligned with R 3.6).

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

Change the path `/home/path/to/docker/version_3.x/original/shared_folder` to the one on your machine. The container
will run for a few seconds, and you should see the outputs (two `.rds` files) in the `shared_folder` folder.

