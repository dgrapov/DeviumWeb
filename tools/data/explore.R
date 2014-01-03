# UI-elements for explore
output$expl_columns <- renderUI({
	cols <- varnames()
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[!isFct]
  if(is.null(cols)) return()

	selectInput("expl_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

output$expl_byvar <- renderUI({
	cols <- varnames()
	# if(sum(input$expl_columns %in% cols) != length(input$expl_columns))  return()

  isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
  if(is.null(cols)) return()

  selectInput(inputId = "expl_byvar", label = "Group by:", choices = cols, selected = NULL, multiple = TRUE)
})

# sq <<- function(x) x^2
expl_functions <- list("Mean" = "mean", "Std. dev" = "sd", "N" = "length", "Max" = "max", "Min" = "min", "Median" = "median", "Mode" = "mode")

output$expl_function <- renderUI({
  if(is.null(input$expl_byvar)) return()
  selectInput(inputId = "expl_function", label = "Apply function(s):", choices = expl_functions, selected = "Mean", multiple = TRUE)

})

output$expl_show_viz <- renderUI({
  if(is.null(input$expl_byvar)) return()
  checkboxInput('expl_show_viz', 'Show plot', value = FALSE)
})

ui_Explore <- function() {
  list(wellPanel(
    uiOutput("expl_columns"),
    uiOutput("expl_byvar"),
    uiOutput("expl_function"),
    returnTextInput("expl_select", "Subset (e.g., mpg > 20 & vs == 1)", ''),
	  div(class="row-fluid",
    	div(class="span6",checkboxInput('expl_show_tab', 'Show table', value = TRUE)),
      div(class="span6", uiOutput("expl_show_viz"))
    )), 
 		helpModal('Explore','explore',includeMarkdown("tools/help/explore.md"))
  )
}

explore <- reactive({
	if(input$datatabs != 'Explore') return()
	if(is.null(input$datasets) || is.null(input$expl_columns)) return(invisible())

	dat <- getdata()

	# if(is.null(input$expl_byvar)) return(getdata()[,input$expl_columns])
	if(sum(input$expl_columns %in% colnames(dat)) != length(input$expl_columns))  return()
	if(is.null(input$expl_byvar)) return(dat[,input$expl_columns])

  if(input$expl_select != '') {
    selcom <- input$expl_select
    selcom <- gsub(" ", "", selcom)
    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

	# if(is.null(input$expl_byvar)) return(getdata()[,input$expl_columns])
	# getdata()[,c(input$expl_byvar,input$expl_columns)]
	dat[,c(input$expl_byvar,input$expl_columns)]

})

explore_plyr <- reactive({

	dat <- explore()
	if(is.null(dat)) return() 

	ply_list <- list()
	for(func in input$expl_function) ply_list[[func]] <- ddply(dat, c(input$expl_byvar), colwise(func))
	ply_list
})


output$expl_data <- renderPrint({
	if(is.null(input$datasets) || is.null(input$expl_columns)) return(invisible())

	dat <- explore()
	if(is.null(dat) || !input$expl_show_tab) return(invisible())

	if(is.null(input$expl_byvar)) {
		isFct <- sapply(dat, is.factor)
		isNum <- sapply(dat, is.numeric)
		isDate <- sapply(dat, is.Date)

		if(sum(isNum) > 0) {
			cat("\nSummarize numeric variables:\n")
			# print(describe(dat[isNum]))
			print(describe(dat[isNum])[,c("n","mean","median","min","max","range","sd","se","skew","kurtosis")])
		}
		if(sum(isFct) > 0) {
			cat("\nSummarize factors:\n")
			print(summary(dat[isFct]))
		}
		if(sum(isDate) > 0) {
			cat("\nSummarize date variables:\n")
			print(summary(dat[isDate]))
		}
	} else {

		for(func in input$expl_function) {
		
			cat("Results grouped by: ", input$expl_byvar, "\n")
			cat("Function used: ", names(which(expl_functions == func)), "\n")
			print(explore_plyr()[[func]])
			cat("\n")
		}
		if(length(input$expl_byvar) > 2 && input$expl_show_viz) cat("Plots will use only the first two 'Group by' variables")
	}
})

expl_plot_width <- function() {
 	# return(input$expl_plot_width)
 	650
}

expl_plot_height <- function() {
 	# return(input$expl_plot_height)
 	400 * length(input$expl_function) * length(input$expl_columns)
}

output$expl_viz <- renderPlot({

	if(is.null(input$datasets) || is.null(input$expl_show_viz)) return()
	if(input$datatabs != 'Explore') return()
	if(!input$expl_show_viz || is.null(input$expl_byvar)) return()

	dat <- explore()
	if(sum(input$expl_columns %in% colnames(dat)) != length(input$expl_columns))  return()

	plots <- list()
	if(length(input$expl_byvar) >= 2) {
		by_var <- input$expl_byvar[1]
		fill_var <- input$expl_byvar[2]
	} else {
		by_var <- fill_var <- input$expl_byvar
	} 

	pnr <- 1
	for(func in input$expl_function) {
		dat_plyr <- explore_plyr()[[func]]
		for(col_var in input$expl_columns) {
			p <- ggplot(data = explore_plyr()[[func]], aes_string(x = by_var, y = col_var, fill = fill_var)) 
			p <- p + geom_bar(stat="identity", position = "dodge", alpha=.3) + ggtitle(paste("Function used:", names(which(expl_functions == func))))
			plots[[pnr]] <- p
			pnr <- pnr + 1
		}
	}

	print(do.call(grid.arrange, c(plots, list(ncol = 1))))

}, width = expl_plot_width, height = expl_plot_height)

observe({
	if(is.null(input$expl_button) || input$expl_button == 0) return()
	isolate({
		# reset the values once the changes have been applied
	  # updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
		# updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "None")
	})
})

#######################################
### When Explore is moved to dplyr
#######################################

# require(devtools)
# install_github("assertthat")
# install_github("dplyr")

# require(assertthat)
# require(dplyr)

# filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
# head(select(hflights, Year:DayOfWeek))
# summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
 
# by_dest <- group_by(hflights, Dest)
# filter(by_dest, ArrDelay == max(ArrDelay))
 
# res <- summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
 
# by_day <- group_by(hflights, Year, Month, DayofMonth)
# by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
# by_month
# summarise(summarise(by_month, delayed = sum(delayed)), delayed = sum(delayed))
# summarise(by_month, delayed = sum(delayed))
 
# by_dest <- group_by(hflights, Dest)
# filter(by_dest, ArrDelay == max(ArrDelay))
# summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
