###############################
# Data normalization (datanormalization)
###############################
# calculate RSD for variables and samples by batch
# allow row and column scaling, transformation

#set batches for complex normalization
output$normalization_batches<-renderUI({
	vars <- varnames()
	selectInput(inputId = "normalization_batches", label = "Batches", choices = c("none"="none",vars),selected="none", multiple = FALSE)
})

#group variable
output$normalization_plot_group1<-renderUI({
	vars <- varnames()
	selectInput(inputId = "normalization_plot_group1", label = "Group 1", choices = vars,selected="none", multiple = TRUE)
})

#group variable
output$normalization_plot_group2<-renderUI({
	vars <- varnames()
	selectInput(inputId = "normalization_plot_group2", label = "Group 2", choices = c("none"="none",vars),selected="none", multiple = FALSE)
})

#select variable for line plot/box plot visualizations
output$normalization_selected_variable <- renderUI({
	# vars <- varnames()
	# isNum <- "numeric" == getdata_class() | "integer" == getdata_class() | "logical" == getdata_class()
	# vars <- vars[isNum]
	vars<-values$normalization_plotting_variable_options
	if(length(vars) == 0) return()
	selectInput(inputId = "normalization_selected_variable", label = "Variable:", choices = vars, selected=vars[1], multiple = FALSE)
})

#select variable for global visualization
output$normalization_plot_global_variables<-renderUI({
	# vars <- varnames()
	# isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	# vars <- vars[isNum]
	vars<-values$normalization_plotting_variable_options # to get update on first load from being unhidden by conditional
	if(length(vars) == 0) return()
	selectInput(inputId = "normalization_plot_global_variables", label = "Variables", choices = vars,selected=vars, multiple = TRUE,selectize=FALSE)
})

#variables to use for normalization
#select variable for global visualization
output$normalization_variables<-renderUI({
	vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
	if(length(vars) == 0) return()
	selectInput(inputId = "normalization_variables", label = "Variables", choices = vars,selected=vars, multiple = TRUE,selectize=FALSE)
})

ui_datanormalization <- function() {
  list(
  	wellPanel(
		div(class="row-fluid", # see css.style for this as well
			div(class="span6",actionButton("datanormalization_calculate", "Calculate")),
			div(class="span6", actionButton("save_datanormalization_results", "Save"))
		),
		conditionalPanel(condition = "input.analysistabs == 'Plots'",
			h3('Plot'),
			tags$details(open="open",tags$summary("Options"),
				selectInput(inputId = "normalization_plot_type", label = "Type", choices = list("global view"="norm_plot_global","line plot"="norm_plot_line","box plot"="norm_plot_box","sample RSD"="norm_plot_sample_rsd","variable RSD"="norm_plot_variable_rsd","batch histogram"="norm_plot_sample_bar","variable histogram"="norm_plot_variable_bar"),selected="global view",multiple=FALSE),
				conditionalPanel(condition = "input.normalization_plot_type == 'norm_plot_global'",
					uiOutput("normalization_plot_global_variables"),
					tags$style(type='text/css', "#normalization_plot_global_variables { height: 150px; padding-bottom: 10px;}"),
					br(),
					numericInput(inputId ="normalization_plot_global_variables_limit", label = "Limit", min = 1, step = 2,value=10)
				),
				#should just update variables above to be multiple=FALSE
				conditionalPanel(condition = "input.normalization_plot_type == 'norm_plot_line'|input.normalization_plot_type == 'norm_plot_box'",	
					uiOutput("normalization_selected_variable")					
				),
				conditionalPanel(condition = "input.normalization_plot_type == 'norm_plot_line'|input.normalization_plot_type == 'norm_plot_box'|input.normalization_plot_type == 'norm_plot_global'",	
					uiOutput("normalization_plot_group1")
				),
				conditionalPanel(condition = "input.normalization_plot_type == 'norm_plot_line'|input.normalization_plot_type == 'norm_plot_box'",	
					uiOutput("normalization_plot_group2")
				),
				conditionalPanel(condition = "input.normalization_plot_type == 'norm_plot_sample_rsd'|input.normalization_plot_type == 'norm_plot_variable_rsd'",	
					div(class="row-fluid", # see css.style for this as well
						div(class="span6",checkboxInput(inputId="norm_plot_show_points", label="Show points", value = TRUE)),
						div(class="span6", checkboxInput(inputId="norm_plot_show_labels", label="Show labels", value = FALSE))
					),	
					numericInput(inputId ="norm_plot_label_size", label = "Label size", min = 0, step = 1,value=4)
				),
				numericInput(inputId ="norm_plot_span", label = "Smooth (span)", min = 0, step = .5,value=.75),
				div(class="row-fluid", # see css.style for this as well
					div(class="span6",numericInput("datanormalization_plot_width", label = "width", min = 0, step = 50, value = 650)),
					div(class="span6", numericInput("datanormalization_plot_height", label = "height", min = 0, step = 50, value = 650))
				),	
				tags$style(type="text/css", "#datanormalization_plot_width {width:75px;}"),
				tags$style(type="text/css", "#datanormalization_plot_height {width:75px;}")
				)
		),	
		h3('Data'),
		tags$details(open="open",tags$summary("Options"),
			uiOutput("normalization_variables"),
			tags$style(type='text/css', "#normalization_variables { height: 150px; padding-bottom: 10px;}")#,
			# br()
		),
		h3('Samples'),
		tags$details(open="open",tags$summary("Options"),
				selectInput(inputId = "normalization_type_sample", label = "Normalization", choices = list("none"="none","mean center"="mean","median center"="median","sum normal"="sum","l2 normal"="l2"),selected="none",multiple=FALSE)
		),
		h3('Variables'),
		tags$details(open="open",tags$summary("Options"),
				selectInput(inputId = "normalization_type_trans_variable", label = "Transformation", choices = list("none"="none","natural logarithm"="ln","logarithm base 10"="log","power"="power"),selected="none",multiple=FALSE),
				conditionalPanel(condition = "input.normalization_type_trans_variable == 'log'|input.normalization_type_trans_variable == 'ln'",
					numericInput(inputId ="norm_log_scalar", label = "Constant", min = 0, step = 5,value=1)
				),
				conditionalPanel(condition = "input.normalization_type_trans_variable == 'power'",	
					numericInput(inputId ="norm_power_scalar", label = "Power", step = 1,value=2)
				),
				selectInput(inputId = "normalization_type_center_variable", label = "Center", choices = list("none"="none","mean"="mean","median"="median"),selected="none",multiple=FALSE),		
				selectInput(inputId = "normalization_type_norm_variable", label = "Normalization", choices = list("none"="none","unit variance"="uv","pareto"="pareto","sum normal"="sum","l2 normal"="l2","range scale"="range_scale"),selected="none",multiple=FALSE)		
		),
		h3('Complex'),
		tags$details(open="open",tags$summary("Options"),
			uiOutput("normalization_batches") # batches for summaries and visualizations
		)	
		
	),
	
	# helpModal('Single mean','singleMean',includeMarkdown("tools/help/singleMean.md"))
	helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))
 	)
}

datanormalization <- reactive({
	if(is.null(getdata())) return()
	#need to remove factor and then add back later after the normalization
	if(!input$tool=="datanormalization") return()
	if(is.null(input$normalization_variables))return()
	tmp.data<-tryCatch(values$normalization_normalized_data<-getdata()[,input$normalization_variables,drop=FALSE],error=function(e){NULL})
	values$normalization_plotting_variable_options<-input$normalization_variables
	if(is.null(input$datanormalization_calculate)) return()

	
	isolate ({
		
		normalization.summary<-list(sample.normalization="none",variable.transformation="none",variable.centering="none",variable.normalization="none")
		#do sample normalizations first
		#then variable transformation
		#then variable normalization
		#then complex normalization
		
		#sample normalization
		if(!is.null(input$normalization_type_sample)){
			if(!input$normalization_type_sample=="none"){
				tmp.data<-scale.data(tmp.data, type=input$normalization_type_sample, dim=1)
				normalization.summary$sample.normalization<-input$normalization_type_sample
			}
		}	
		#variable transformation
		if(!is.null(input$normalization_type_trans_variable)){
			if(!input$normalization_type_trans_variable=="none"){
				if(input$normalization_type_trans_variable=="log"){
					tmp.data<-log10(tmp.data+input$norm_log_scalar)
					normalization.summary$variable.transformation<-paste0("log base 10 of data + ",input$norm_log_scalar)
				}
				if(input$normalization_type_trans_variable=="ln"){
					tmp.data<-log(tmp.data+input$norm_log_scalar)
					normalization.summary$variable.transformation<-paste0("natural log of data + ",input$norm_log_scalar)
				}
				if(input$normalization_type_trans_variable=="power"){
					tmp.data<-data.frame(tmp.data^input$norm_power_scalar)
					normalization.summary$variable.transformation<-paste0("power transformation to exponent ",input$norm_power_scalar)
				}
			}
		}	

		#variable centering
		if(!is.null(input$normalization_type_center_variable)){ 
			if(!input$normalization_type_center_variable=="none"){
				tmp.data<-scale.data(tmp.data, type=input$normalization_type_center_variable, dim=2)
				normalization.summary$variable.centering<-paste0(input$normalization_type_center_variable," centered")
			}
		}	
		#variable normalization
		if(!is.null(input$normalization_type_norm_variable)){ # should avoid this everywhere up top
			if(!input$normalization_type_norm_variable=="none"){
				tmp.data<-scale.data(tmp.data, type=input$normalization_type_norm_variable, dim=2)
				normalization.summary$variable.normalization<-paste0(input$normalization_type_norm_variable," normalized")
			}
		}
		#format method details
		methods<-data.frame(methods=t(as.data.frame(normalization.summary)))
		rownames(methods)<-c(paste0(rownames(methods),"      ")) # add white space for improved visibility # should make things like this in MD
		
		#normalized data
		values$normalization_normalized_data<-tmp.data
		
		#define replicated sample ids for batch based performance summary
		if(is.null(input$normalization_batches)){
			batch<-data.frame(batch=rep(1,nrow(tmp.data)))
		} else {	
			if(input$normalization_batches=="none") {
				batch<-data.frame(batch=rep(1,nrow(tmp.data)))
			} else {
				batch<-tryCatch(getdata()[,input$normalization_batches,drop=FALSE],error=function(e){rep(1,nrow(tmp.data))}) #not sure why get error  here when going from the PCA module
			}
		}
		
		values$normalization_full_performance<-calc.mRSD(data=data.frame(tmp.data),batch=batch) 
		values$normalization_summary_performance<-summarize.performance(obj=values$normalization_full_performance,sig.figs=2)
		values$normalization_results<-list(methods=methods,performance=values$normalization_summary_performance)
	})	
})

summary.datanormalization <- function(result) {
	if(is.null(values$normalization_results)) return("Please calculate first.")
	result
}

plot.datanormalization <- function(result) {
	if(input$analysistabs == 'Summary'){return()}
	if(is.null(input$normalization_plot_type)) return()
	if(is.null(values$normalization_normalized_data)) return()
	# if(is.null(values$normalization_normalized_data)) values$normalization_normalized_data<-getdata() # default to the raw data if no calculations have been triggred

	.theme<- theme(
			axis.line = element_line(colour = 'gray', size = .75), 
			panel.background = element_blank(), 
			plot.background = element_blank(),
			axis.text.x = element_text(size = 10),
			axis.text.y = element_text(size = 10),
			axis.title.x = element_text(size=15),
			axis.title.y = element_text(size=15),
			legend.key = element_blank()
		 )
	
	if(any(input$normalization_plot_type%in%c('norm_plot_line','norm_plot_box','norm_plot_global'))){
		
		#group1
		if(is.null(input$normalization_plot_group1)){
			group1<-NULL
		} else {
			if(input$normalization_plot_group1=="none"){ 
				group1<-NULL
			} else {	
				group1<-getdata()[,input$normalization_plot_group1,drop=FALSE]
				if(ncol(group1)>1) {
					if(!input$normalization_plot_type=="norm_plot_global"){
						.names<-paste(colnames(group1),collapse="|")
						group1<-data.frame(factor(join.columns(group1)))
						colnames(group1)<-.names
					}	
				}	
			}
		}
			
		#group2
		if(is.null(input$normalization_plot_group2)){
			group2<-NULL
		} else {
			if(input$normalization_plot_group2=="none"){ 
				group2<-NULL
			} else {
				group2<-getdata()[,input$normalization_plot_group2,drop=FALSE]
			}	
		}	
		
		variable<-values$normalization_normalized_data[,input$normalization_selected_variable,drop=FALSE]	
		
		if(input$normalization_plot_type=='norm_plot_line') {
			tryCatch(summary.lineplot(val=data.frame(variable),groups=group1,view.split=group2,theme=.theme,span=input$norm_plot_span),error=function(e) {empty.plot(e)})
		}
		if(input$normalization_plot_type=='norm_plot_box') {
			tryCatch(summary.boxplot2(val=data.frame(variable),groups=group1,split.on=group2,theme=.theme,span=input$norm_plot_span),error=function(e) {empty.plot(e)})
		}
		
		if(input$normalization_plot_type=='norm_plot_global'){	
			
			tmp.data<-values$normalization_normalized_data[,input$normalization_plot_global_variables,drop=FALSE]
			if(input$normalization_plot_global_variables_limit>ncol(tmp.data)) limit<-ncol(tmp.data) else limit<-input$normalization_plot_global_variables_limit
			tmp.data<-tmp.data[,1:limit,drop=FALSE]
			if(is.null(group1)) group1<-data.frame(overall=factor(rep("",nrow(tmp.data))))
			
			#create MetaboAnalyst like data summary boxplots
			tryCatch(summary.boxplot(tmp.data,group1),error=function(e) {empty.plot(e)})
		}	
	}
	
	#performance summary plots
	if(!is.null(values$normalization_full_performance)){
		if(input$normalization_plot_type=="norm_plot_sample_rsd"){
			RSD.means.plot(obj=list(values$normalization_full_performance),type="sample",name=c(""),theme=.theme,label=input$norm_plot_show_labels, label.size=input$norm_plot_label_size,points=input$norm_plot_show_points,span=input$norm_plot_span)
				# extra=list(guides(fill = guide_legend(show = FALSE), color = guide_legend(show = FALSE),group=guide_legend(show = FALSE) ))) # cant't turn off legend extra=list(guides(fill = guide_legend(show = FALSE) ))
			
		}
		if(input$normalization_plot_type=="norm_plot_variable_rsd"){
			RSD.means.plot(obj=list(values$normalization_full_performance),type="variable",name=c(""),theme=.theme,label=input$norm_plot_show_labels, label.size=input$norm_plot_label_size,points=input$norm_plot_show_points,,span=input$norm_plot_span)
			#extra=list(guides(fill = guide_legend(show = FALSE), color = guide_legend(show = FALSE),group=guide_legend(show = FALSE) )))
		}
	} else return(empty.plot("Please calculate first."))
	
	if(!is.null(values$normalization_summary_performance)){	
		if(input$normalization_plot_type=="norm_plot_sample_bar"){
			RSD.counts.plot(obj=list(values$normalization_summary_performance),show="sample",name=c(""),theme=.theme,ylabel="number of batches",extra=list(guides(fill = guide_legend(show = FALSE) ))) 
		}
		if(input$normalization_plot_type=="norm_plot_variable_bar"){
			RSD.counts.plot(obj=list(values$normalization_summary_performance),show="variable",name=c(""),theme=.theme,ylabel="number of metabolites",extra=list(guides(fill = guide_legend(show = FALSE) )))
		}
	} else 	return(empty.plot("Please calculate first."))
}

#plot dimensions
datanormalization_plot_width<-reactive({
	if(!input$tool=="datanormalization") return()
	if(is.null(input$datanormalization_plot_width))  return()
	values$plot$'plotWidth'<-input$datanormalization_plot_width
})

datanormalization_plot_height<-reactive({
	if(!input$tool=="datanormalization") return()
	if(is.null(input$datanormalization_plot_height))  return()
	values$plot$'plotHeight'<-input$datanormalization_plot_height

})

#update variables for plotting based on input to normalization
#having problems because controls are hidden by conditional formatting
#update only happens if control is visible...
#------------------------
observe({
	
	if(!is.null(input$normalization_variables)) {
		if(input$analysistabs == 'Plots'&input$tool=="datanormalization"){
				# if(!is.null(input$normalization_plot_global_variables)) {
			vars<-input$normalization_variables
			updateSelectInput(session, inputId="normalization_plot_global_variables", label = "Variables",choices = vars,selected=vars)
		}		# }
	# })		
	}

})


observe({
	if(!is.null(input$normalization_variables)) {
		if(input$analysistabs == 'Plots'&input$tool=="datanormalization"){
			vars<-input$normalization_variables
			updateSelectInput(session, inputId="normalization_selected_variable", label = "Variable:",choices = vars,selected=vars[1])
		}	
	}	
})
#------------------------

#watcher to normalize when action button is pressed
# not sure why this is not respected?
observe({
	if(!input$tool=="datanormalization") return()
	if(is.null(input$datanormalization_calculate)) return()
	if(input$datanormalization_calculate==0) return()
	input$datanormalization_calculate_plot # recalculate on plot
	isolate({
		datanormalization()
	})	
})	

#watcher for plot dimensions
observe({
	if(!input$tool=="datanormalization") return()
	datanormalization_plot_width()
	datanormalization_plot_height()
})

#save results
observe({ 
	if(is.null(input$save_datanormalization_results)) return ()
	if(input$save_datanormalization_results==0) return()
	isolate({
		if(is.null(values$normalization_normalized_data)) return()
		name<-paste0(input$datasets,"_normalized")
		#should bind with the rest of the data set to keep
		left.out<-varnames()[!varnames()%in%colnames(values$normalization_normalized_data)]
		if(is.null(left.out)) return.data<-values$normalization_normalized_data else return.data<-data.frame(getdata()[,left.out],values$normalization_normalized_data)
		values[[name]]<-return.data
		values$datasetlist <- unique(c(values$datasetlist,name))
	})
})	
