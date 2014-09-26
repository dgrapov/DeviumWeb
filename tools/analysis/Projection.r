###############################
# PCA
###############################

#convert vector to named list (may not need to do this)
namel<-function (vec){
		tmp<-as.list(vec)
		names(tmp)<-as.character(unlist(vec))
		tmp
	}
	
#UI
output$pca_variables <- renderUI({
	vars <- varnames()
	# isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	# vars <- vars[isNum]
  if(length(vars) == 0) return()
  # selectInput(inputId = "pca_var", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE)
  selectInput(inputId = "pca_var", label = "Variables:", choices = vars, selected=vars, multiple = TRUE, selectize=FALSE)
})

output$pca_groups<-renderUI({
	vars <- varnames()
	isNum <- !"numeric" == getdata_class() & !"integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
 
  selectInput(inputId = "pca_groups", label = "", choices = vars, multiple = TRUE)
})

output$PCs<-renderUI({
		
		maxPCs<-ncol(getdata())
		numericInput("pca_pcs", "Number of Components", 2, min = 2, max = maxPCs)
	})
	
output$pca_x_axis<-renderUI({
		numericInput("pca_x_axis", "x-axis", value=1, min = 1, max = input$pca_pcs)
	})
	
output$pca_y_axis<-renderUI({
		numericInput("pca_y_axis", "y-axis", value=2, min = 1, max = input$pca_pcs)
	})	

#make UI	
ui_pca <- function() {
	list(
		wellPanel(
		# conditionalPanel(condition = "input.analysistabs=='Plots'",downloadButton('Download_pca_plot', label = "Download plot")),
			# br(),
			h4('Calculate'),
			tags$details(open="open",tags$summary("Options"),
				uiOutput("pca_variables"), # variables
				tags$style(type='text/css', "#pca_var { height: 200px; padding-bottom: 10px;}"),
				br(),
				uiOutput("PCs"),
				checkboxInput("pca_center","Center",TRUE),
				selectInput("pca_scaling","Scale", list(none = "none", "unit variance" = "uv", pareto = "pareto"),selected="unit variance"),		
				selectInput("pca_method","Method", namel(listPcaMethods())),
				selectInput("pca_cv","cross-validation",list (none = "none", Q2 =  "q2"),selected="Q2")
			),
			h4('Plot'),
			tags$details(tags$summary("Options"),	
				selectInput("pca_plot","Plot type",list ("Scree Plot"= "screeplot" , Scores =  "scores", Loadings= "loadings",Biplot="biplot"),selected="Scores"),
				conditionalPanel(condition = "input.pca_plot != 'screeplot'",
							tags$details(open="open",tags$summary("Groups"),
								# h5('Groups'),
								conditionalPanel(condition = "input.pca_plot != 'loadings'",
									uiOutput("pca_groups")
								),
								selectInput("pca_group_bounds","Boundary:",list (none = "none", ellipse =  "ellipse",polygon = "polygon"),selected="ellipse"),
								conditionalPanel(condition = "input.pca_group_bounds != 'none'",
									numericInput("pca_group_alpha", "Transparency:",value=.35, min = 0,max=1,step=.1)
								)
							),
						tags$details(tags$summary("Components"),
						# h5('Components'),
							uiOutput("pca_x_axis"),
							uiOutput("pca_y_axis")
						),
						tags$details(tags$summary("Points"),
						# h5('Points'),
							numericInput("pca_size", "Size:", value=5, min = 0,step=1),
							numericInput("pca_point_alpha", "Transparency", value=.75, min = 0,max=1,step=.1)
						),
						tags$details(tags$summary("Labels"),
						# h5('Labels'),
							checkboxInput("pca_show_labels","Show",TRUE),
							numericInput("pca_label_font_size", "Size:", value=5, min = 0,step=1),
							textInput("pca_legend_label", "Legend title", value = "auto")	
						)	
				),	
				tags$details(tags$summary("More options"),
							br(),
								downloadButton('Download_pca_plot', label = "Download plot")
							)	
			),
			h4('Save'),
				tags$details(tags$summary("Objects"),
					selectInput("pca_result_obj","",choices=c("-----" = ""),selected = "-----",multiple=TRUE),#,values[["opls_objects"]]$return_obj_name),
					actionButton("save_pca_result_obj", "Save")
				)	
		)#,
		#adding modal alters the markup rendering	
		# helpModal('Single mean','singleMean',includeMarkdown("tools/help/singleMean.md"))
		# br(),
		# helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))
	)
}

#save PCA results
observe({
	
	#meta data for scores
	if(is.null(input$datasets)) return()
			
	#update GUI
	if(is.null(values$pca_result_objects)) return()
	updateSelectInput(session, "pca_result_obj", choices = values$pca_result_objects)
	
	# #save
	if(is.null(input$save_pca_result_obj) || input$save_pca_result_obj == 0) return()
	isolate({
		values$datasetlist <- unique(c(values$datasetlist,values$pca_result_objects))
	})	
})	


#text output
summary.pca <- function(result) {
	result
}

#main fxn
pca <- reactive({

	ret_text <- "Please select 2 or more variables of type numeric or integer."
	if(is.null(input$pca_var)) return(ret_text)
	# # if(is.null(inChecker(c(input$sm_var)))) return(ret_text) not sure

	# return("nothing to see")
	pca.data <- getdata()[,input$pca_var,drop=FALSE]
	#get factors to add to sample meta for output
	
	# adapted from another devium
	pca.inputs<-list()
	pca.inputs$pca.data<-afixln(pca.data) # convert everything to numeric
	pca.inputs$pca.algorithm<-input$pca_method
	pca.inputs$pca.components<-input$pca_pcs
	pca.inputs$pca.center<-input$pca_center
	pca.inputs$pca.scaling<-input$pca_scaling
	pca.inputs$pca.cv<-input$pca_cv # currently not used
	# c(pca.inputs,data)
	pca.results<-values$pca.results<-devium.pca.calculate(pca.inputs,return="list",plot=F)
	# return(pca.results)
	
	
	# #save outputs #should add button to trigger
	# isolate({
	pca_result_objects<-list()
	name<-paste0(input$datasets,"_PCA_sample_info")
	#add index, and Y for visualizations
	#bind with any factors in the data
	
	diagnostics<-data.frame(values$pca.results$pca.diagnostics)
	tmp.obj<-data.frame(index=c(1:nrow(values$pca.results$pca.scores)),scores=values$pca.results$pca.scores,diagnostics)
	meta<-fixlr(getdata(),.remove=F)
	tmp.obj<-data.frame(meta,tmp.obj)
	# rownames(tmp.obj)<-rownames(get(input$datasets))
	rownames(tmp.obj)<-rownames(getdata())
	# values[[name]]<-tmp.obj
	values[["pca_objects"]]$pca_sample_info_name<-name
	values[[name]]<-tmp.obj	
	pca_result_objects<-c(pca_result_objects,name)
	# values$datasetlist <- unique(c(values$datasetlist,name))
	# })
	
	# isolate({
	name<-paste0(input$datasets,"_PCA_variable_info")
	#add index, and Y for visualizations #should use expression set type object and carry around row an col meta in extra items
	tmp.obj<-data.frame(index=c(1:nrow(values$pca.results$pca.loadings)),loadings=values$pca.results$pca.loadings)
	# values[[name]]<-tmp.obj
	# values$datasetlist <- unique(c(values$datasetlist,name))
	values[["pca_objects"]]$pca_variable_info_name<-name
	values[[name]]<-tmp.obj	
	pca_result_objects<-c(pca_result_objects,name)
	values$pca_result_objects<-unlist(unique(pca_result_objects))
	# })	
	
	return(pca.results)
	
})

#plot
plot.pca <- function(result) {
	#set colors (getting ugly!)
	pca_groups<-input$pca_groups
	if(is.null(pca_groups)){pca_groups<-"none"}
	if(!pca_groups=="none"){color<- getdata()[,pca_groups,drop=FALSE]} else {color<-NULL}
	if(input$pca_plot=="loadings"){ color<-NULL}
	if(!is.null(color)){
		cnames<-paste(colnames(color),collapse="|")	
		color<-data.frame(color=join.columns(color))
		colnames(color)<-cnames
	}	
	if(input$pca_legend_label=="auto"){ legend.name=NULL }else { legend.name<-input$pca_legend_label }
	plot.PCA(pca=values$pca.results,xaxis=input$pca_x_axis,yaxis=input$pca_y_axis, results = input$pca_plot,
	group.bounds=input$pca_group_bounds,size=input$pca_size,color=color, label=input$pca_show_labels, 
	legend.name =  legend.name,font.size=input$pca_label_font_size,alpha=input$pca_point_alpha,g.alpha=input$pca_group_alpha)
		
}

#save plot
save.plot.pca.name<-reactive(function(){
	# paste(input$datasets,input$plot_type,sep="-")
	sprintf("%s_%s.%s",input$datasets,input$pca_plot,'pdf')
	})

output$Download_pca_plot<-downloadHandler(
      filename = save.plot.pca.name,
    # a function that actually opens a connection to a pdf and print the result to it
    content = function(FILE=NULL) {
	
		# make.ggplot()
		# ggsave(file="plot.pdf")#FILE
		
	    pdf(file=FILE, onefile=T, width=12,height=8)
		# print(make_PC_Plot())
		plot.pca()
		dev.off()
	}
)
