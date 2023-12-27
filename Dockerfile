# Use the official R base image
FROM rocker/r-ver:4.3.2

# Install necessary system dependencies
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libudunits2-dev \
    libproj-dev \
    libgdal-dev \
    libgeos-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    libv8-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libgsl-dev \
    libmagick++-dev \
    libtesseract-dev \
    libleptonica-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install required R packages
RUN install2.r --error --skipinstalled \
    shiny \
    plotly \
    readr \
    sf \
    arrow \
    dplyr \
    ggplot2 \
    tidyr \
    classInt \
    glue \
    lubridate \
    gridExtra \
    gganimate \
    gifski \
    tidyverse

# Copy the Shiny app files into the container
COPY app.R /srv/shiny-server/app.R
COPY /shiny/ /srv/shiny-server/shiny/

EXPOSE 3838

# Run the Shiny app when the container starts
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/')"]
