shinybootstrap2::withBootstrap2({shinyUI(
  pageWithSidebar(

    # Using a navbar rather than headerPanel to display app title
    headerPanel(''),
    sidebarPanel(

		#busy indicator
		tagList(
			tags$head(
			  tags$link(rel="stylesheet", type="text/css",href="style.css"),
			  tags$script(type="text/javascript", src = "js/busy.js")
			)
		),
		div(class = "busy",  
		  # p("Calculation in progress.."), 
		  img(src="images/progressbird.gif")
		),
		  # tags$head(tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript') # ),
		  includeCSS('www/style.css'),
		  tags$head(tags$script(src = "js/jquery-ui.min.js")),
		  # tags$head(tags$script(src = "js/lr.js")),
		  # includeHTML("www/js/sel2.js"),
		  # includeHTML('www/js/lr.js'), 
		  getTool("tool"),

		  wellPanel(
			uiOutput("datasets")
		  ),

		  # find the appropriate UI
		  uiOutput("ui_finder")
		),

    mainPanel(	
		# conditionalPanel(condition = "!is.null(values$DATA_message)",	
			bsAlert(inputId = "DATA_anchor"),
		# ),
		
		conditionalPanel(condition = "input.datasets != ''",
			conditionalPanel(condition = "input.tool == 'data'",
				uiOutput("ui_data_tabs")
			)
		),
		conditionalPanel(condition = "input.tool != 'data'",
		  uiOutput("ui_analysis_tabs")
		)
    )
  )
)
})
