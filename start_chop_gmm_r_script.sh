#!/bin/bash

docker start chop_gmm_container
docker exec chop_gmm_container git pull origin master
docker exec chop_gmm_container Rscript chop_gmm_model.R $@
docker stop chop_gmm_container
