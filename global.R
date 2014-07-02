# avoid breaks in R-output print and show JSON packets transferred
# over websockets
# options(error = recover)
# options(shiny.reactlog=TRUE)
# options(shiny.trace=TRUE)

# options(width = 150, digits = 3)
options(digits = 3)

# creating a reactivevalues store 
values <- reactiveValues()

values[['running_app_local']] <- FALSE
if(Sys.getenv('SHINY_PORT') == "") {
  # options(shiny.maxRequestSize=1000000*1024^2)
  # no limit to filesize locally
  options(shiny.maxRequestSize=-1)
  values[['running_app_local']] <- TRUE
}

# main install happens in update.R now. this is just to 
options(repos = c(CRAN = "http://cran.rstudio.com"))
source('libs.R', local = TRUE)

# # available <- suppressWarnings(suppressPackageStartupMessages(sapply(libs, require, character.only=TRUE)))
# available <- suppressWarnings(sapply(libs, require, character.only=TRUE))
# inst.libs <- libs[available == FALSE]
# if(length(inst.libs) != 0) {
  # install.packages(inst.libs, dependencies = TRUE)
	# # suppressWarnings(suppressPackageStartupMessages(sapply(inst.libs, require, character.only=TRUE)))
  # suppressWarnings(sapply(inst.libs, require, character.only=TRUE))
# }

#load all R packages
check.get.packages(libs)


panderOptions('digits',3)

# binding for a text input that only updates when the return key is pressed
returnTextInput <- function(inputId, label, value = "") {
  tagList(
    singleton(tags$head(tags$script(src = "js/returnTextInputBinding.js"))),
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class = "returnTextInput")
  )
}

# binding for a sortable list of variables or factor levels
html_list <- function(vars, id) {

  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  paste0(hl, "</ul>")
}

# binding for a sortable list of variables or factor levels
returnOrder <- function(inputId, vars) {
  tagList(
    # singleton(tags$html(includeHTML('www/sort.html'))),
    # singleton(tags$head(tags$script(src = 'http://code.jquery.com/ui/1.10.3/jquery-ui.js'))),
    singleton(tags$head(tags$script(src = 'js/sort.js'))),
    singleton(includeCSS("www/sort.css")),
    HTML(html_list(vars, inputId)),
    tags$head(tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });")))
  )
}


# unloading because it messes with method of some other packages
# would prefer to use import From but ...
#----!!!IMPORTANT !#--- may cause radyant problems
# detach("package:Hmisc", unload=TRUE) # radyant problems with this
#----------


# mtcars <- NULL
robj <- load("data/mtcars.rda") 

# setting up a few standard datasets to play with 
# mtcars$vs <- as.factor(mtcars$vs)
# mtcars$am <- as.factor(mtcars$am)

values[["mtcars"]] <- data.frame(get(robj[1]))
values[["mtcars_descr"]] <- get(robj[2])

# diamonds <- NULL
robj <- load("data/diamonds.rda") 
values[["diamonds"]] <- data.frame(get(robj[1]))
values[["diamonds_descr"]] <- get(robj[2])

values$datasetlist <- c("mtcars", "diamonds")
#using datasets in main code n

# loading list of data.frame in the car package
# listPackData <- function(packs) {
# 	libnames <- c('')
# 	lib <- c('')
# 	for(ipack in packs) {
# 		ilib <- data(package = ipack)$results
# 		libnames <- c(libnames, paste(ilib[,'Package'],ilib[,'Item'], sep = '-'))
# 		lib <- c(lib,ilib[,'Item'])
# 	}
# 	names(lib) <- libnames
# 	as.list(lib)
# }

# packDataSets <- listPackData(c('AER','Ecdat'))

# pds <- list()
# for(pd in 2:length(packDataSets)) {
# 	data(list = packDataSets[[pd]])
# 	dat <- get(packDataSets[[pd]])
# 	if(!is.null(colnames(dat))) {
# 		pds <- c(pds,packDataSets[pd])
# 	}
# }
# # packDataSets <- packDataSets[-which(packDataSets == "DutchSales")]
# # packDataSets <- c(list(''=''),pds)
# packDataSets <- c('',pds)
# save(packDataSets, file = '~/Desktop/packDataSets.rda')

# load('data/packDataSets.rda')
# lastLoaded <- "" 		

getTool <- function(inputId) {
  tagList(
    tags$head(tags$script(src = "js/navbar.js")),
    tags$html(includeHTML('www/navbar.html'))
  )
}

# function to render .Rmd files into html on-the-fly
includeRmd <- function(path){
  if (!require(knitr))
    stop("knitr package is not installed")
  if (!require(markdown))
    stop("Markdown package is not installed")
  shiny:::dependsOnFile(path)
  contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
  html <- knitr::knit2html(text = contents, fragment.only = TRUE, options = "base64_images")
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'), 
  trigger=c('click', 'hover', 'focus', 'manual')) {

  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1], 
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
  )
}

helpModal <- function(title, link, content) {
  html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal'>×</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <a data-toggle='modal' href='#%s' class='icon-question-sign'></a>", link, title, content, link)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}


#get Devium R-scripts for analysis functions
# source("http://pastebin.com/raw.php?i=JVyTrYRD")
source("http://pastebin.com/raw.php?i=UyDBTA57")


values$debug_on<-TRUE # turn FALSE to remove debugging
# #source local directory to load devium (DEVELOPMENT ONLY)
#source.local.dir<-function(wd){
#	o.dir<-getwd()
#	setwd(wd)
#	files<-dir()[unique(c(agrep(".r",dir()),agrep(".R",dir())))]
#	lapply(1:length(files),function(i) {tryCatch(source(files[i]),error=function(e){paste0("can't load-->",files[i])})
#	})
#	setwd(o.dir)
#}

#source.local.dir("C:/Users/D/Dropbox/Devium/devium/R")

# Simulate a big data-file
# n <- 200000
# n.var <- 100
# bigsimdat <- data.frame(matrix(rnorm(n*n.var),nrow = n, ncol <- n.var))
# save(bigsimdat,file = "data/bigsimdat.rda")
