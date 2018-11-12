# BFAST Explorer v0.0.3

## Description

**BFAST Explorer** is a [Shiny](https://shiny.rstudio.com/) app, developed using R and Python, designed for the analysis of *Landsat Surface Reflectance* time series pixel data.

Three change detection algorithms - **bfastmonitor**, **bfast01** and **bfast** - are used in order to investigate temporal changes in trend and seasonal components, via breakpoint detection.

If you encounter any bugs, please create an [issue](https://github.com/almeidaxan/bfast-explorer/issues) or send a message to almeida.xan@gmail.com and/or nathmenini@gmail.com.

## Usage

Currently, this tool only supports UNIX-like systems (no Windows OS) due to the integration between R and Python.

In order to successfully run this tool, apart from having R and Python installed, you need the following:

* Request access to use GEE (https://earthengine.google.com/signup/)
* Install and set up GEE Python API (https://developers.google.com/earth-engine/python_install)

## Citation

To cite BFAST Explorer in publications, please use

> Alexandre Almeida, Nathalia Menini, Jan Verbesselt, Ricardo Torres (2018). BFAST Explorer: An Effective Tool for Time Series Analysis. In: 2018 IEEE International Geoscience and Remote Sensing Symposium (IGARSS). 22-27 July 2018. Valencia, Spain. DOI: 10.1109/IGARSS.2018.8517877.

Or, alternatively, the corresponding BibTeX entry

```
@inproceedings{,
   author = {Alexandre Almeida and Nathalia Menini and Jan Verbesselt and Ricardo Torres},
   title = {BFAST Explorer: An Effective Tool for Time Series Analysis},
   booktitle = {2018 IEEE International Geoscience and Remote Sensing Symposium (IGARSS)},
   location = {Valencia, Spain},
   eventdate = {2018-07-22/2018-07-27},
   doi = {10.1109/IGARSS.2018.8517877}
}
```
