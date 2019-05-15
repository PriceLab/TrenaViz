FROM rocker/shiny:3.5.1
RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'V8', 'DT'))"
USER root
RUN apt-get update
RUN apt-get install -y xdg-utils
WORKDIR /home/trena/app
COPY app.R .
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/home/trena/app/app.R', port=3838, host='0.0.0.0')"]
