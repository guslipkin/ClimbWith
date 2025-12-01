FROM rocker/verse:4.5.0
RUN apt-get update && apt-get install -y  gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libicu-dev libpng-dev libproj-dev libssl-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'install.packages("renv", repos = "https://packagemanager.posit.co/cran/latest")'
RUN R -e 'renv::restore(prompt = FALSE)'
RUN R -e 'renv::install(".", prompt = FALSE)'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');ClimbWith::run_app();"
