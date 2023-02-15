# User `rocker` pre-built `R` containers.
ARG VERSION="4.2.2"
FROM rocker/r-ver:$VERSION as base

# Install needed packages and git.
RUN apt-get update \ 
&& export DEBIAN_FRONTEND=noninteractive \
&& apt-get -y install \
libgit2-dev \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libxt-dev

RUN apt-get -y git 

# Checkout git repository.
WORKDIR /workdir 
RUN git clone https://github.com/diagnosticdrivinginc/CHOP_GMM_standalone.git .

# Install `checkpoint` library.
RUN Rscript -e "install.packages('checkpoint')"

# Sleep until called upon.
CMD sleep infinity
