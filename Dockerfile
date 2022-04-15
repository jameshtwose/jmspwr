FROM rocker/shiny:4.0.4
# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinythemes", \
              "plotly", \
              "ICC.Sample.Size", \
              "WebPower", \
              "pmsampsize", \
              "sjstats", \
              "ggthemes", \
              "shiny.router", \
              "lme4", \
              "Rcpp", \
              "pwr", \
              "plyr", \
              "sjlabelled", \
              "usethis", \
              "shinytest", \
              "reshape", \
              "ggplot2"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'
WORKDIR /home/shinyusr
COPY . .
CMD Rscript deploy.R