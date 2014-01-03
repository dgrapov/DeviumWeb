# UI-elements for transform
output$tr_columns <- renderUI({
	cols <- varnames()
	selectInput("tr_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

output$tr_reorder_levs_rui <- renderUI({
	if(is.null(input$tr_columns)) return()
	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
  if(!isFct) return()
  dat <- getdata()
  returnOrder("tr_reorder_levs", levels(dat[,input$tr_columns[1]]))	
})

revFactorOrder <- function(x) {
	x <- as.factor(x)
	factor(x, levels=rev(levels(x)))
}

standardize_1sd <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(as.numeric(scale(x)))
}

centerVar <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(x - mean(x, na.rm = TRUE))
	x
}

medianSplit <- function(x) cut(x, breaks=quantile(x,c(0,.5,1)), include.lowest=TRUE, labels=c("Below","Above"))

decileSplit <- function(x) cut(x, breaks=quantile(x,seq(0,1,.1)), include.lowest=TRUE, labels=seq(1,10,1))

shift <- function(x,shift_by){
	# from http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))

  if (length(shift_by) > 1)
    return(sapply(shift_by,shift, x = x))

  # prefer to have positive number create lags as normal in ts-literature
  shift_by <- -shift_by

  out <- NULL
  abs_shift_by = abs(shift_by)
  if (shift_by > 0)
    out <- c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0)
    out <- c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out <- x
  out
}

sq <<- function(x) x^2
inv <<- function(x) 1/x
st <<- standardize_1sd
st2 <<- rescale
cent <<- centerVar 
msp <<- medianSplit
dec <<- decileSplit
# lagx <<- shift
fct <<- as.factor
num <<- as.numeric
int <<- as.integer
ch <<- as.character
rfct <<- revFactorOrder

# d <<- as.Date
d_mdy <<- function(x) as.Date(mdy(as.character(x)))
d_dmy <<- function(x) as.Date(dmy(as.character(x)))
d_ymd <<- function(x) as.Date(ymd(as.character(x)))

# trans_options <- list("None" = "none", "Remove" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize (1-sd)" = "st1", 
# trans_options <- list("None" = "", "Remove" = "remove", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize (1-sd)" = "st1", 
# "Standardize (2-sd)" = "st2","Invert" = "inv", "Bin 2" = "bin2", "Bin10" = "bin10", "As factor" = "fct", "Rev factor order" = "rfct", "As number" = "num", "As character" = "ch", 

trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize" = "st", 
	"Invert" = "inv", "Median split" = "msp", "Deciles" = "dec", "As factor" = "fct",  "As number" = "num", "As integer" = "int", "As character" = "ch",
	"As date (mdy)" = "d_mdy", "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd")

trans_types <- list("----------" = "", "Change" = "change", "Create" = "create", "Clipboard" = "clip", "Recode" = "recode", "Rename" = "rename", "Reorder columns" = "reorder_cols", "Reorder levels" = "reorder_levs", "Remove" = "remove")

ui_Transform <- function() {
	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  list(wellPanel(
    uiOutput("tr_columns"),

   	# radioButtons("tr_changeType", "", c("Change" = "change", "Create" = "create", "Clipboard" = "clip", "Recode" = "recode", "Rename" = "rename", "Reorder" = "reorder", "Remove" = "remove"), selected = "Change"),
    selectInput("tr_changeType", "Transformation type:", trans_types, selected = "----------"),
    conditionalPanel(condition = "input.tr_changeType == 'change'",
	    selectInput("tr_transfunction", "Change columns:", trans_options)
    ),
    conditionalPanel(condition = "input.tr_changeType == 'create'",
	    returnTextInput("tr_transform", "Create (e.g., x = y - z):", '')
	    # textInput("tr_transform", "Create (e.g., x = y - z):", ''), 
  	  # actionButton("tr_transform_sub", "Go")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'clip'",
      # actionButton('pasteClipData', 'Paste data')
    	HTML("<label>Paste from Excel:</label>"),
	    tags$textarea(id="tr_copyAndPaste", rows=3, cols=5, "")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'recode'",
	    returnTextInput("tr_recode", "Recode (e.g., lo:20 = 1):", '')
	    # textInput("tr_recode", "Recode (e.g., lo:20 = 1):", ''), 
  	  # actionButton("tr_recode_sub", "Go")
    ),

    conditionalPanel(condition = "input.tr_changeType == 'rename'",
	   	returnTextInput("tr_rename", "Rename (separate by ','):", '')
	   	# textInput("tr_rename", "Rename (separate by ','):", '')
    ),

    # actionButton("transfix", "Edit variables in place") # using the 'fix(mtcars)' to edit the data 'inplace'. Looks great from R-ui, not so great from Rstudio
    conditionalPanel(condition = "input.tr_changeType != ''",
	    actionButton("addtrans", "Save changes")
	  ),

    conditionalPanel(condition = "input.tr_changeType == 'reorder_cols'",
    	br(),
    	HTML("<label>Reorder (drag-and-drop):</label>"),
	    returnOrder("tr_reorder_cols", varnames())
    ),

    conditionalPanel(condition = "input.tr_changeType == 'reorder_levs'",
    	br(),
    	HTML("<label>Reorder (drag-and-drop):</label>"),
	    # returnOrder("tr_reorder_levs", varnames())
	    uiOutput("tr_reorder_levs_rui")
    )
  	), 
		helpModal('Transform','transform',includeMarkdown("tools/help/transform.md"))
	)
}

transform_main <- reactive({

	if(input$datatabs != 'Transform') return()
	if(is.null(input$tr_changeType) || input$tr_changeType == '') return()
	if(is.null(input$datasets)) return()

	dat <- getdata()

	if(input$tr_changeType == 'reorder_cols') {
    if(is.null(input$tr_reorder_cols)) {
      ordVars <- colnames(dat)
 	  } else {
   	  ordVars <- input$tr_reorder_cols
    }
 	  return(dat[,ordVars, drop = FALSE])
  }

	if(!is.null(input$tr_columns)) {

		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		if(input$tr_transfunction != '') {
			cn <- c(colnames(dat),paste(input$tr_transfunction,colnames(dat), sep="."))
			dat <- cbind(dat,colwise(input$tr_transfunction)(dat))
			colnames(dat) <- cn
		}
	}

	if(!is.null(input$tr_columns) & input$tr_changeType == 'reorder_levs') {
    if(!is.null(input$tr_reorder_levs)) {
    	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
		  if(isFct) dat[,input$tr_columns[1]] <- factor(dat[,input$tr_columns[1]], levels = input$tr_reorder_levs)
    }
  }

	if(input$tr_changeType ==  'recode') {
		if(input$tr_recode != '') {

			recom <- input$tr_recode
			recom <- gsub(" ", "", recom)
			recom <- gsub("\"","\'", recom)

			newvar <- try(do.call(car::recode, list(dat[,input$tr_columns[1]],recom)), silent = TRUE)
			if(!is(newvar, 'try-error')) {

				cn <- c(colnames(dat),paste("rc",input$tr_columns[1], sep="."))
				dat <- cbind(dat,newvar)
				colnames(dat) <- cn
				return(dat)
			}
		}
	}

	if(input$tr_changeType == 'clip') {
		if(input$tr_copyAndPaste != '') {
			cpdat <- read.table(header=T, text=input$tr_copyAndPaste)
			cpname <- names(cpdat)
			if(sum(cpname %in% colnames(dat)) > 0) names(cpdat) <- paste('cp',cpname,sep = '.')
			if(is.null(input$tr_columns)) return(cpdat)
			if(nrow(cpdat) == nrow(dat)) dat <- cbind(dat,cpdat)
		}
	}

	if(input$tr_changeType == 'rename') {
		if(!is.null(input$tr_columns) && input$tr_rename != '') {
			rcom <- unlist(strsplit(gsub(" ","",input$tr_rename), ","))
			rcom <- rcom[1:length(input$tr_columns)]
			names(dat)[1:length(rcom)] <- rcom
		}
	}

	if(input$tr_changeType == 'create') {
		if(input$tr_transform != '') {
			recom <- input$tr_transform
			recom <- gsub(" ", "", recom)
			recom <- gsub("\"","\'", recom)

			fullDat <- getdata()
			newvar <- try(do.call(within, list(fullDat,parse(text = recom))), silent = TRUE)

			if(!is(newvar, 'try-error')) { 
				nfull <- ncol(fullDat)
				nnew <- ncol(newvar)

				# this won't work properly if the transform command creates a new variable
				# and also overwrites an existing one
				if(nfull < nnew) newvar <- newvar[,(nfull+1):nnew, drop = FALSE]
				if(is.null(input$tr_columns)) return(newvar)
				cn <- c(colnames(dat),colnames(newvar))
				dat <- cbind(dat,newvar)
				colnames(dat) <- cn
			} else if(is.null(input$tr_columns)) {
				# the 'create' command did not compile so if there were
				# no variables selected show ... nothing
				# print(paste0("Create command:", recom, "did not create a new variable. Please try again."))
			 	# updateTextInput(session = session, inputId = "tr_transform", label = "Create (e.g., y = x - z):", '')
				return(paste0("Create command: ", recom, " did not create a new variable. Please try again."))
				# return()
			}
		}
	}

	dat
})

output$transform_data <- reactive({

	dat <- transform_main()
	if(is.null(dat)) return()
	if(is.character(dat)) return(dat)

	dat <- data.frame(date2character_dat(dat))
	nr <- min(nrow(dat),10)
	dat <- dat[1:nr,, drop = FALSE]

	html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
	html <- paste(html, '<label>10 rows shown. See View-tab for details.</label>') 
	html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  # Encoding(html) <- 'UTF-8'
  html

})

output$transform_summary <- renderPrint({

	dat <- transform_main()
	if(is.null(dat)) return(invisible()) 			# ...

	isFct <- sapply(dat, is.factor)
	isNum <- sapply(dat, is.numeric)
	isDate <- sapply(dat, is.Date)
	# isChar <- sapply(getdata(), is.character)

	if(sum(isNum) > 0) {
		cat("\nSummarize numeric variables:\n")
		# print(describe(dat[,isNum])[,c("n","mean","sd","median","min","max","range","skew","kurtosis","se")])
		print(describe(dat[,isNum])[,c("n","mean","median","min","max","range","sd","se","skew","kurtosis")])
	}
	if(sum(isFct) > 0) {
		cat("\nSummarize factors:\n")
		print(summary(dat[,isFct]))
	}
	if(sum(isDate) > 0) {
		cat("\nSummarize date variables:\n")
		print(summary(dat[,isDate]))
	}

	# print(getwd())
	
})

observe({
	if(is.null(input$addtrans) || input$addtrans == 0) return()
	isolate({
		dat <- transform_main()
		if(is.null(dat)) return()
		if(is.character(dat)) return(dat)

		if(input$tr_changeType == 'remove') {
			changedata(addColName = colnames(dat))
		} else if(input$tr_changeType == 'rename') {
			changedata_names(input$tr_columns, colnames(dat))
		} else if(input$tr_changeType == 'reorder_cols') {
	  	values[[input$datasets]] <- values[[input$datasets]][,input$tr_reorder_cols]
	  } else {
			changedata(dat, colnames(dat))
		}


		# reset the values once the changes have been applied
	 	updateTextInput(session = session, inputId = "tr_transform", label = "Create (e.g., y = x - z):", '')
	 	updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
	 	updateTextInput(session = session, inputId = "tr_rename", label = "Rename (separate by ','):", value = '')
	 	updateTextInput(session = session, inputId = "tr_copyAndPaste", label = "", '')
		updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "None")
	})
})

