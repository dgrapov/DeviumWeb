
[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_buynowCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=VF2GAHBNPKFRE)

# [DeviumWeb](http://spark.rstudio.com/dgrapov/DeviumWeb/#)
=========
<b>D</b>ynamic Multivariat<b>E</b> Data Analysis and <b>VI</b>s<b>U</b>alization Platfor<b>M</b> based on [Radiant](https://github.com/mostly-harmless/radiant) by Vincent Nijs.

![logo](other/generic_logo.png)
![demo](other/devium1.0.gif)

###[Screenshots](other/screenshots.md)

###Information

Version: 0.4 (1/25/2015)

News: [See the latest features and fixes.](https://github.com/dgrapov/DeviumWeb/blob/master/NEWS.md)

### Download
* To download the current development version, click the 'Download ZIP' button at the top right of this page. 
* Older versions 
[DeviumWeb v0.3.1](https://sourceforge.net/projects/devium/files/DeviumWeb/DeviumWeb%20v0.3.1.zip/download) (10/09/14)

### Installation

- Required: [R](http://cran.rstudio.com/) (v3.1.1)
- Required: [Shiny](http://www.rstudio.com/shiny/) (v0.10.2.2 to v0.11.1)
- Required: A modern browser (e.g., Chrome (recommended), Firefox, or Safari).

Unzip the file and then start Devium by running the following code in the R console (change file path if not unzipped to the desktop).

  
	#-------------
	#Windows
	#-------------
  
	library(shiny)
	shiny::runApp('~/../Desktop/DeviumWeb-master/DeviumWeb-master')

	#-------------
 	#Mac
 	#-------------
 	library(shiny)
 
	shiny::runApp('~/Desktop/DeviumWeb-master/DeviumWeb-master')

### Citation
If you find DeviumWeb useful please cite this tool as stated below.

Dmitry Grapov. (2014) DeviumWeb: Dynamic Multivariate Data Analysis and Visualization Platform. v0.4 (1/25/2015)

[![DOI](https://zenodo.org/badge/7400/dgrapov/DeviumWeb.svg)](http://dx.doi.org/10.5281/zenodo.17459)

### TODO

- convert to R package to enable easier loading
- add help files
- add network modules

### License

DeviumWeb is licensed under the [AGPLv3](http://www.tldrlegal.com/l/AGPL3). The help files are licensed under the creative commons attribution, non-commercial, share-alike license <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">CC-NC-SA</a>. See the files listed below for additional details.

- COPYING - DeviumWeb license (AGPLv3 and CC-NC-SA)
- NOTICE - Copyright notices for additional included software

As a summary, the AGPLv3 license requires, attribution, include copyright and license in copies of the software, state changes if you modify the code, and disclose all source code. Details are in the COPYING file.


&copy; Dmitry Grapov (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>
