################################################################
# general functions used in radyant
################################################################
changedata <- function(addCol = list(NULL), addColName = "") {
	# change data as specified
	if(addColName[1] == "") return()
  # isolate ensures no reactive dependencies are used
  # isolate({
  	if(length(addCol) == 1 && is.null(addCol[[1]])) {
  		return(values[[input$datasets]][,addColName] <- addCol)
  	} else if(nrow(getdata()) == nrow(addCol)) {
	  	return(values[[input$datasets]][,addColName] <- addCol)
  	} 
  	# else {
	  # 	return(values[[input$datasets]][,addColName] <- addCol)
	  # }
  # })
}

changedata_names <- function(oldnames, newnames) {

	upnames <- colnames(values[[input$datasets]])
	upnames[which(upnames %in% oldnames)] <- newnames
	return(colnames(values[[input$datasets]]) <- upnames)
}

inChecker <- function(tocheck) {

	# checking if variables are in the selected dataset
	for(var in tocheck) {
		if(!var %in% varnames()) return(NULL)
	}

	return('Good stuff')
}

getdata <- reactive({
	values[[input$datasets]]

	# if(is.null(input$data_filter)) {
	# 	return(values[[input$datasets]])
	# } else {
	# 	return(values[[input$datasets]])
		# dat <- values[[input$datasets]]
		# dat[dat[,input$data_filter], ]
	# }
})

getdata_class <- reactive({
	# don't use isolate here or values won't change when the dataset is changed
	cls <- sapply(getdata(), function(x) class(x)[1])
	gsub("ordered","factor", cls)
})

# varnames <- function() {
varnames <- reactive({
	# if(is.null(input$datasets)) return()
	dat <- getdata_class()
	vars <- names(dat)
	names(vars) <- paste(vars, " {", dat, "}", sep = "")
	vars
})

date2character <- reactive({

	dat <- getdata()
  isDate <- c("Date" == getdata_class())
	# needed because xtable doesn't like dates
	dat[,isDate] <- sapply(dat[,isDate], as.character)
	dat
})

date2character_dat <- function(dat) {

  isDate <- c(sapply(dat, is.Date))
	# needed because xtable doesn't like dates
	dat[,isDate] <- sapply(dat[,isDate], as.character)
	dat
}

output$columns <- renderUI({
	cols <- varnames()
	selectInput("columns", "Select columns to show:", choices  = as.list(cols), selected = names(cols), multiple = TRUE)
})

################################################################
# general functions used in devium
################################################################
#helper for data transposing mechanism
rdy.t<-function(obj){
  list<-dimnames(obj)
  names<-lapply(seq(list), function(i){
    tmp<-check.fix.names(fixlc(list[[i]]),ok.chars=c(".","_"))
    test<-!is.na(as.numeric(tmp))
    paste(ifelse(test,"X",""),tmp,sep="")           
  })
  out<-as.data.frame(obj)
  dimnames(out)<-names
  return(data.frame(out))
}


varnames2 <- function() {
	if(is.null(input$datasets)) return()

	dat <- getdata()
	cols <- colnames(dat)
	names(cols) <- paste(cols, " {", sapply(dat,class), "}", sep = "")
	cols
}




################################################################
# Output controls for the Summary and Plots tabs
# The tabs are re-used for various tools. Depending on the tool
# selected by the user the appropriate analysis function 
# is called.
#
# Naming conventions: The reactive function to be put in the
# code block above must be of the same name as the tool
# in the tools drop-down. See global.R for the current list
# of tools (and tool-names) 
################################################################

# Generate output for the summary tab
# output$summary <- renderText({
output$summary <- renderPrint({
	if(is.null(input$datasets) || input$tool == 'data') return()

	# get the summary function for currently selected tool and feed
	# it the output from the related analysis reactives 
	# get-function structure is used because there may be many
	# sets of tools that will have the same output structure

	# call analysis reactive
	result <- get(input$tool)()

	if(is.character(result)) {
		# used when no analysis is conducted (e.g., no variables selected yet)
		# ret <- cat(result,"\n")
		cat(result,"\n")
	} else {
		# pass analysis results to the summary function
		f <- get(paste("summary",input$tool,sep = '.'))
		# ret <- f(result)
		f(result)
	}

	# query <- parseQueryString(session$clientData$url_search)
  # print(query)
	# ret

})

plotHeight <- function(height = 650) {

 	# height <- try(get(input$tool)()[['plotHeight']], silent = TRUE)
	height <- try(values$plot$'plotHeight')
	if(is(height, 'try-error') || is.null(height)) {
		return(650)
	} else {
		return(height)
	}
}

plotWidth <- function(width = 650) {

 	# width <- try(get(input$tool)()[['plotWidth']], silent = TRUE)
	width <- try(values$plot$'plotWidth')
	if(is(width, 'try-error') || is.null(width)) {
		return(650)
	} else {
		return(width)
	}
}


# Generate output for the plots tab
output$plots <- renderPlot({

	# plotting could be expensive so only done when tab is being viewed
	if(input$tool == 'data' || input$analysistabs != 'Plots') return()

	# call analysis reactive
	result <- get(input$tool)()
	if(!is.character(result)) {
		# pass analysis results to the plotting function
		f <- get(paste("plot",input$tool,sep = '.'))
		f(result)
	} else {
		# used when no analysis is conducted (e.g., no variables selected yet)
		plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = "")
	}

}, width=plotWidth, height=plotHeight)

