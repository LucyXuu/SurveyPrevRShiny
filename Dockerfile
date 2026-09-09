FROM rocker/shiny:4.5.1

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    curl \
    g++ \
    gdal-bin \
    git \
    gfortran \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libglpk40 \
    libharfbuzz-dev \
    libjpeg-dev \
    libpng-dev \
    libproj-dev \
    libssl-dev \
    libtiff5-dev \
    libudunits2-dev \
    libuv1-dev \
    libxml2-dev \
    make \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server

## Restore packages BEFORE copying the rest of the app, so the (slow) package
## layer is cached and only rebuilds when renv.lock changes.
COPY renv.lock renv.lock
COPY renv renv
COPY .Rprofile .Rprofile

## - repos.override: install from the image's default repositories (rocker
##   points these at Posit Package Manager Linux *binaries*), instead of the
##   source-package CRAN URLs recorded in the lockfile. This makes the restore
##   fast and avoids compilation failures of pinned old versions.
## - GITHUB_PAT (BuildKit secret): authenticates GitHub downloads (MICSprev
##   etc.) so CI builds don't die on GitHub API rate limits.
## - INLA is excluded from restore and installed from its own repository.
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN --mount=type=secret,id=github_token \
    if [ -s /run/secrets/github_token ]; then \
        export GITHUB_PAT="$(cat /run/secrets/github_token)"; \
    fi \
    && R -e "cat('repos in use:\n'); print(getOption('repos'))" \
    && R -e "options(renv.config.repos.override = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest')); renv::restore(lockfile = '/srv/shiny-server/renv.lock', exclude = 'INLA', prompt = FALSE)" \
    && R -e "options(timeout = 600); install.packages('INLA', repos = c(INLA = 'https://inla.r-inla-download.org/R/testing', CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/latest'), type = 'source')"

# Keep the restored project library from the cached dependency layer.
COPY --exclude=renv . .

# Match the library used during the dependency restore.  With DESCRIPTION
# present, renv otherwise treats this as a package project and selects a
# different user-cache library at runtime.
ENV RENV_PATHS_LIBRARY=/srv/shiny-server/renv/library

EXPOSE 3838

CMD ["R", "-e", "setwd('/srv/shiny-server'); app <- source('app.R', chdir = TRUE)$value; shiny::runApp(app, host = '0.0.0.0', port = 3838)"]
