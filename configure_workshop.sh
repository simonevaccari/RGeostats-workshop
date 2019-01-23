#!/bin/bash

# This script aims at configuring Ellip Jupyter Notebooks server
# for INTAROS RGeostats workshop

# Ask confirmation
echo ""
echo "## Welcome to RGeostats Workshop configuration script! ##"
echo ""
echo "You are going to overwrite old workshop stuff from the current directory."
read -p "Are you sure (Y/N)? " -n 1 -r
echo    # (optional) move to a new line
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit
fi

# Python modules
pip install netCDF4
pip install numpy
pip install shapely
pip install urllib3

# R packages
for package in r-rcpp r-maps r-mapdata r-mapproj r-png r-fields r-maptools r-proj4 r-ncdf4 r-raster r-rgdal r-gsl r-misc3d; do
  conda install -y -n python2 $package
done

# RGeostats and RIntaros packages
Rscript_cmd="/opt/anaconda/envs/python2/bin/Rscript"
$Rscript_cmd -e 'install.packages("http://cg.ensmp.fr/rgeos/DOWNLOAD/RGeostats_11.2.8_linux64.tar.gz")'
$Rscript_cmd -e 'install.packages("http://cg.ensmp.fr/rgeos/DOWNLOAD/RIntaros_1.1_linux64.tar.gz")'

# Remove previous stuff in the current directory
rm -rf rgeostats.free.fr
rm -rf lib
rm -f workshop.tar.gz
rm -f workshop_rgeostats_imr.ipynb
rm -f estimate.ipynb
rm -f geostats_course.ipynb
rm -f imr_case_study.ipynb
rm -f download_data.py
rm -f estimate.R
rm -f reading.r
rm -f functions.r
rm -f *.Rmd
rm -f *.pdf
rm -f imr_data_0_to_100m.csv
rm -f imr_data_by_10m.csv

# Workshop programs (overwrite)
wget http://rgeostats.free.fr/doc/Files/workshop.tar.gz
tar xvf workshop.tar.gz
rm workshop.tar.gz

# IMR Datasets
wget http://rgeostats.free.fr/doc/Files/imr_data_0_to_100m.csv
mv imr_data_0_to_100m.csv data/

wget http://rgeostats.free.fr/doc/Files/imr_data_by_10m.csv
mv imr_data_by_10m.csv data/

