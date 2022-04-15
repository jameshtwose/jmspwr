FROM rocker/shiny:4.0.4
# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "shiny", \
              "shinythemes", \
              "shiny.router", \
              "plotly", \
              "ggthemes", \
              "plyr", \
              "reshape", \
              "rsconnect"), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R