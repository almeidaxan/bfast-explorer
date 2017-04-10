## Description

**BFAST Explorer** is a [Shiny](https://shiny.rstudio.com/) app, developed using R and Python, designed for the analyis of *Landsat Surface Reflectance* time series pixel data.

Three change detection algorithms - **bfastmonitor**, **bfast01** and **bfast** - are used in order to investigate temporal changes in trend and seasonal components, via breakpoint detection.

If you encounter any bugs, please create an issue on this page or send a message to almeida.xan@gmail.com.

## Usage

Currently, this tool only supports UNIX-like systems (no Windows OS) due to the integration between R and Python.

In order to successfully run this tool, apart from having a version from R and Python installed, you need the following: 

* Request access to use GEE (https://earthengine.google.com/signup/)
* Install and set up GEE Python API (https://developers.google.com/earth-engine/python_install)
