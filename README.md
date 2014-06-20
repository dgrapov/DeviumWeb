[DeviumWeb](http://spark.rstudio.com/dgrapov/DeviumWeb/#)
=========
<b>D</b>ynamic Multivariat<b>E</b> Data Analysis and <b>VI</b>s<b>U</b>alization Platfor<b>M</b> based on [Radiant](https://github.com/mostly-harmless/radiant) by Vincent Nijs.

![logo](other/generic_logo.png)
![demo](other/devium1.0.gif)

### Download
- [DeviumWeb v1.0.0](https://sourceforge.net/projects/devium/files/DeviumWeb/DeviumWeb%20v1.0.zip/download) (02/2014)
- To download the current development version, click the 'Download ZIP' button at the top right of this page. 

### Installation

- Required: [R](http://cran.rstudio.com/) (v3.0.1)
- Required: [Shiny](http://www.rstudio.com/shiny/) (v0.8.0)
- Required: A modern browser (e.g., Chrome, Firefox, or Safari). Internet Explorer is not supported.

Unzip the file and then start Devium by running the following code in the R console (change file path if not unzipped to the desktop).

	# on windows
	library(shiny)
	shiny::runApp('~/../Desktop/DeviumWeb-master/DeviumWeb-master')

 	# on mac
 	library(shiny)
	shiny::runApp('~/Desktop/DeviumWeb-master/DeviumWeb-master')

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
