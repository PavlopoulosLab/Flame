FROM r-base
#Description of the image
LABEL description="Flame Docker Image"
LABEL maintainer="Fotis Baltoumas<baltoumas@fleming.gr>"
LABEL version="2.0"
#set the timezone
ENV TZ Europe/Athens
#copy the Rprofile.site file and containing the shiny port and CRAN repo url
COPY Rprofile.site /usr/lib/R/etc/

#
RUN apt update
RUN apt install -y libssl-dev libcurl4-openssl-dev libxml2-dev libfontconfig1-dev firefox file libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

# install required R packages
COPY install_packages.R /root/install_packages.R
RUN Rscript /root/install_packages.R
# copy the Flame directory to the VM - CHANGE "./Flame2.0" to the path containing the Flame2.0 folder
COPY ./Flame2.0/ /root/Flame/
#expose the shiny port
EXPOSE 3838

#set the default command to run, ie "R"
CMD ["R", "-e", "shiny::runApp('/root/Flame/')"]
