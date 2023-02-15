# User `rocker` pre-built `R` containers.
ARG VERSION="4.2.2"
FROM rocker/r-ver:$VERSION as base

# Install needed packages and git.
RUN apt-get update \ 
&& apt-get -y install \
libgit2-dev \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libxt-dev

RUN apt-get -y git 

# Checkout git repository
WORKDIR /workdir 
RUN git clone 
