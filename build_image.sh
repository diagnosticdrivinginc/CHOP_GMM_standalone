#!/bin/bash

# Builds a Docker image.
docker build --no-cache -t chop_gmm_image .

# Creates a Docker container.
docker create --name chop_gmm_container chop_gmm_image
