
This repository contains a number of different scripts I have writted to perform small needed pre-processing tasks for a couple of different SNAP projects involving sea ice data. It is kept up-to-date based in the activity in the realm of sea ice data needs for SNAP.

#### rasterize_seaIce_data.r

This script contains a procedure for converting the Dutch Meteorological Institute (DMI) sea ice concentration maps from the mid-1900s which have been hand digitized from scanned maps from the [NSIDC](http://nsidc.org/data/docs/noaa/g02203-dmi/). 

It takes as input the digitized map in shapefile format, which contains many unique dates of information, parses out the data related to the unique dates and subsets / rasterizes the shapefile using a standard rasterization procedure, where the value for a given pixel is the value intersecting its centroid.  The output file format is an LZW Compressed GeoTiff file with a *Pacific-Centered* Latitude / Longitude reference system using the WGS 1984 Datum.

#### NSIDC_SeaIceConcentration_convert_flatBinary_toTIFF.r

This is a very simple script that takes as input [NSIDC Sea Ice Concentration Data](http://nsidc.org/data/docs/daac/nsidc0051_gsfc_seaice.gd.html) (gsfc version) and converts it from its flat binary format into GeoTiff format, which is the SNAP standard data format for digital raster map data. These were the monthly time step data in its raw format, which was the only available at the time.


#### RotatePole_NetCDF_ClimateDataOperators.py

A short script written in Python to convert the CMIP5 Sea Ice Concentration Data from the models to a usable format.  They were each in a slightly different rotated pole coordinate system, which was able to be modified using [Climate Data Operators (CDO)](https://code.zmaw.de/projects/cdo).  This is simply a loop written in python to exploit the command line software for a large number of models from the [Earth System Grid Data Portal](https://www.earthsystemgrid.org/home.htm), run by the National Center for Atmospheric Research (NCAR).

#### Tracy_SeaIce_NSIDC_rasterize_t42grid.r

A messy script of a bunch of R code generated while assisting a student employee at SNAP tasked with perfoming sea ice data analyses.  This should be viewed as mostly garbage, but keeping it around for at least a few more months before tossing it, in case it again becomes useful.
