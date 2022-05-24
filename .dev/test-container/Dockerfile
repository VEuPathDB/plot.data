FROM rocker/r-ver:4.0.4

## Set a default user. Available via runtime flag `--user rserve`
## User should also have & own a home directory (for rstudio or linked volumes to work properly).
RUN useradd rserve \
	&& mkdir /home/rserve \
	&& chown rserve:rserve /home/rserve

RUN apt-get update && apt-get install -y \
	libglpk-dev \
	libxml2-dev

## install libs
### Rserve
RUN R -e "install.packages('Rserve', version='1.8-9', repos='http://rforge.net')"

### CRAN
RUN R -e "install.packages('bit64')"
RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('remotes')"
RUN R -e "install.packages('Rcpp')"
RUN R -e "install.packages('dotenv')"
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('sloop')"
RUN R -e "install.packages('scales')"

RUN R -e "remotes::install_github('VEuPathDB/veupathUtils', 'v1.0.1')"
RUN R -e "remotes::install_github('VEuPathDB/plot.data','v2.1.4')"
RUN R -e "remotes::install_github('VEuPathDB/microbiomeComputations', 'v1.0.2')"


# possibly for RStudio
EXPOSE 8787


WORKDIR /home/rserve
