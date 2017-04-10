## Description

**BFAST Explorer** is an exploratory tool developed in R, Python and [Shiny](https://shiny.rstudio.com/), designed to allow easy and fast time series analysis from *Landsat Surface Reflectance* pixel data. This tool uses *Google Earth Engine (GEE)* as a medium to access remote sensing data and perform some preprocessing, such as cloud filtering.

After the data is downloaded, three change detection algorithms (**bfastmonitor**, **bfast01** and **bfast**) can be applied to the time series in order to investigate changes in the trend and/or seasonal components over time.

If you encounter any bugs please create an issue on this page or send a message to almeida.xan@gmail.com.

## Usage

Currently, this tool only supports UNIX-like systems (no Windows OS) due to the integration between R and Python.

In order to successfully run this tool, apart from having R and Python installed, you need the following: 

* Request access to use GEE (https://earthengine.google.com/signup/)
* Install and set up GEE Python API (https://developers.google.com/earth-engine/python_install)
