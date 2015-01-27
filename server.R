
shinyServer(function(input, output, session) {

	#Devium functions
	source('devium.R', local = TRUE)
	
	#radyant functions
	source('radyant.R', local = TRUE)

	# source data & analysis tools
	flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE, modifiedOnly = FALSE) # w/o modifiedOnly = FALSE won't reload when browser refreshes
	flist_data <- sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)

	# find the appropriate UI
	output$ui_finder <- renderUI({
  	if(input$tool == "data") {
  		if(!is.null(input$datatabs)) get(paste0('ui_',input$datatabs))()
		} else {
		  if(!is.null(input$tool)) get(paste0('ui_',input$tool))()
		}
	})

	# data tabs
	output$ui_data_tabs <- renderUI({
		tabsetPanel(id = "datatabs",
			tabPanel("Overview",  # was called manage and uses manage ui
			# #add collapsable panels with shinBS !!! only works one time then goes non-responsive??
				# # bsCollapse(multiple = TRUE, open = NULL, id = "overview_collapse",
					# bsCollapsePanel("Table", dataTableOutput("manage_data_table"), id="overview_collapse_1", value="test1")
				# # )
					# # bsCollapsePanel("Summary", verbatimTextOutput("ManageDataStr"), id="overview_collapse_2", value="test2"),
					# # bsCollapsePanel("Description",
						# # conditionalPanel(condition = "input.man_add_descr == false",
							# # HTML(dataDescriptionOutput())
						# # ),
						# # conditionalPanel(condition = "input.man_add_descr == true",
							# # HTML("<label>Add data description:</label>"),
							  # # tags$textarea(id="man_data_descr", rows="10", cols="12", dataDescriptionOutput('md'))
						# # ),
						# # id="overview_collapse_3", value="test3"
					# # )			
					
				# # )
				hr(),
				tags$details(open="open",tags$summary(tags$div(icon("fa fa-table"),tags$label(style="font-size: 30px;","Table"),style="font-size: 30px; color: #EF3732;")),
					dataTableOutput("manage_data_table")
				),
				hr(),
				tags$details(tags$summary(tags$div(icon("fa fa-list-alt"),tags$label(style="font-size: 30px;","Summary"),style="font-size: 30px; color: #EF3732;")),
					verbatimTextOutput("ManageDataStr")
				),
				hr(),
				tags$details(tags$summary(tags$div(icon("fa fa-book"),tags$label(style="font-size: 30px;","Description"),style="font-size: 30px; color: #EF3732;")),
					conditionalPanel(condition = "input.man_add_descr == false",
							HTML(dataDescriptionOutput())
					),
					conditionalPanel(condition = "input.man_add_descr == true",
						HTML("<label>Add data description:</label>"),
						  tags$textarea(id="man_data_descr", rows="10", cols="12", dataDescriptionOutput('md'))
					)
				)
			 
			),

			tabPanel("Transform", htmlOutput("transform_data"), verbatimTextOutput("transform_summary")),
			#should test for internet if not show local else show iframe (when website built)
# 			tabPanel("About", includeRmd("about.Rmd")),
      tabPanel("About", includeMarkdown("about.md")),
			# conditionalPanel(condition = "values.debug_on == 'TRUE'",
			tabPanel("Debug", verbatimTextOutput("debug")) # turn on return of input and values
			# )
		)
	})

	#updateCollapse(session, id = "overview_collapse", multiple = TRUE, open = "overview_collapse", close = NULL) # unresponsive after first laod
	
		
	dataDescriptionOutput <- function(ret = 'html') {

 		dataDescr <- paste0(input$datasets,"_descr")

 		# text box loses focus some how - sort of works
		# if(is.null(input$man_data_descr) || input$man_data_descr == "") {
		# 	text <- values[[dataDescr]]
		# } else { 
		# 	if(!is.null(input$man_add_descr) && input$man_add_descr == TRUE) {
		# 		text <- input$man_data_descr
		# 	} else {
		# 		text <- values[[dataDescr]]
		# 	}
		# }

		text <- values[[dataDescr]]
 		if(is.null(text)) {
 			return("")
 		} else {
			if(ret == 'md') {
				return(text)
			} else {
				# html <- markdownToHTML(text = text)
				# markdownToHTML(text = text)
				html <- suppressWarnings(markdownToHTML(text = text, stylesheet="www/fancyTab.css"))
			 	Encoding(html) <- 'UTF-8'
			 	html
			}
 		}
	}

	fancyTableOutput <- function() {

	  fancyTab <- try(get(paste0(input$tool,'_fancy_tab'))(), silent = TRUE)
  	if(!is(fancyTab, 'try-error')) {
  		if(is.null(fancyTab)) return("")
			html <- markdownToHTML(text = fancyTab, stylesheet="www/fancyTab.css")
			# html <- markdownToHTML(text = fancyTab)
			html <- sub("<table>","<table class='table table-condensed'>", html)
			# Encoding(html) <- 'UTF-8'
			html
		} else {
			""
		} 
	}

	# analysis output tabs can be customized in the tools files
	output$ui_analysis_tabs <- renderUI({
	  tabs <- try(get(paste('ui_',input$tool,'_tabs', sep=""))(), silent = TRUE)
  	if(is(tabs, 'try-error')) {
  		return(tabsetPanel(id = "analysistabs",
	  	  tabPanel("Summary", HTML(fancyTableOutput()), verbatimTextOutput("summary")),
	    	# conditionalPanel(condition = "input.tool != 'visualize'", 
				tabPanel("Plots", plotOutput("plots", height = "100%"))
			# )#,
			# may want to alter based on input tool
	    	# conditionalPanel(condition = "input.tool == 'visualize'", 
				# tabPanel("Plots", plotOutput("plots", width = input$viz_plot_width, height=input$viz_plot_height))
			# )
		)		
		)
			# tabPanel("Help", includeHTML(paste0("tools/help/",input$tool,".html"))))
	  } else {
  		return(tabs)
	  }
	})

	#UPDATE DATA tab alert
	observe({
		if(is.null(input$datasets)) return()
		message<-NULL
		dat<-getdata()
		if(sum(is.na(dat))>0) message<-paste("The data has",sum(is.na(dat)),"missing value(s), which should be imputed prior to some analyses.")
		
		if(is.null(message)) return()
			createAlert(session, inputId = "DATA_anchor",
				message = message,
				type = "danger",
				dismiss = TRUE,
				block = TRUE,
				append = FALSE
			)
		
	})
	

	
	# From Joe Cheng's post at:
	# https://groups.google.com/forum/?fromgroups=#!searchin/shiny-discuss/close$20r/shiny-discuss/JptpzKxLuvU/boGBwwI3SjIJ
	# session$onSessionEnded(function() {
 #  	q("ask")
 #  })
})
