###############################
# Data summary
###############################
#fix:
# output for 1 row, 
# control of formatted choices
# colnames export

output$summary_groups<-renderUI({
	vars <- varnames()
	isNum <- "numeric" == getdata_class()
 	vars <- vars[!isNum]
  # if(length(vars) == 0) return()
  selectInput(inputId = "sum_group", label = "Grouping variables:", choices = c("none"="none",vars),selected="none", multiple = TRUE)
})

output$summary_variables <- renderUI({
	vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class() | "logical" == getdata_class()
 	vars <- vars[isNum]
	if(length(vars) == 0) return()
	selectInput(inputId = "sum_var", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE, selectize=FALSE)
	# selectInput(inputId = "sum_var", label = "Data:", choices = vars,  multiple = TRUE)
})

ui_datasummary <- function() {
  list(
  	wellPanel(
		uiOutput("summary_variables"), # variables
		tags$style(type='text/css', "#sum_var { height: 150px; padding-bottom: 10px;}"),
		br(),
		uiOutput("summary_groups"), # groups
		conditionalPanel(condition = "input.analysistabs == 'Summary'",
			selectInput(inputId = "sum_stat_vars", label = "Statistics", choices = list("mean"="sum_mean","median"="sum_median","sum"="sum","standard deviation"="sum_sd","minimum"="sum_min","maximum"="sum_max","RSD"="rsd","Fold change"="FC"), selected = c("mean","standard deviation"), multiple =TRUE),
			radioButtons(inputId = "sum_formatresults", label = "Format results:", c("Separate" = "separate", "Formatted" = "formatted"), selected = "Separate"),
			actionButton("save_stats_summary_results", "Save results")			
		),
		conditionalPanel(condition = "input.analysistabs == 'Plots'",
			numericInput(inputId ="summary_limit_variables", label = "max variables", min = 1, step = 1,value=50),
			div(class="row-fluid", # see css.style for this as well
				div(class="span6",numericInput("summary_plot_width", label = "width", min = 0, step = 50, value = 650)),
				div(class="span6", numericInput("summary_plot_height", label = "height", min = 0, step = 50, value = 650))
			),
			tags$style(type="text/css", "#summary_plot_width {width:75px;}"),
			tags$style(type="text/css", "#summary_plot_height {width:75px;}")
			#doesn't work to get in line side to side
			# fluidRow(
				# column(4,
					# numericInput(inputId ="summary_limit_variables", label = "max variables", min = 1, step = 1,value=50),
					# numericInput(inputId = "summary_plot_width", "width", min = 0,  value = 650, step = 10),
					# numericInput(inputId = "summary_plot_height", "height", min = 0,  value = 850, step = 10),
					# tags$style(type="text/css", "#summary_plot_width     {   display:inline-block}"),
					# tags$style(type="text/css", "#summary_plot_height     {   display:inline-block}")
				# )
			# )	
					
		)
		
	),
	
	# helpModal('Single mean','singleMean',includeMarkdown("tools/help/singleMean.md"))
	helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))
 	)
}

summary.datasummary <- function(result) {
	result
}

plot.datasummary <- function(result) {
	if(input$analysistabs == 'Summary'){return("")}
	
	if(input$sum_group=="none"){
		factor<-data.frame(data=rep("1",nrow(getdata())))
	} else {
		factor<-getdata()[,input$sum_group,drop=FALSE]
	}
	if(is.null(factor)){
		return("Select data with factors or create factors for the current data set.")
	}
	data <- getdata()[,input$sum_var,drop=FALSE]
	
	if(!is.null(input$summary_limit_variables)){
	if(input$summary_limit_variables>ncol(data)) limit<-ncol(data) else limit<-input$summary_limit_variables
	
	data<-data[,1:limit]
	}
	
	#create MetaboAnalyst like data summary boxplots
	summary.boxplot(data,factor)
	
}

#plot dimensions
summary_plot_width<-reactive({
	if(is.null(input$summary_plot_width))  return()
	values$plot$'plotWidth'<-input$summary_plot_width

})

summary_plot_height<-reactive({
	if(is.null(input$summary_plot_height))  return()
	values$plot$'plotHeight'<-input$summary_plot_height

})

#watcher 
observe({
	if(!input$tool=="datasummary") return()
	summary_plot_width()
	summary_plot_height()
})

datasummary <- reactive({

	# ret_text <- "This analysis requires the variables to be of type numeric or integer and group of type factor . Please select another dataset."
	# if(is.null(input$sum_groups)) return(ret_text)
	# if(is.null(input$sum_var)) return(ret_text)
	# # if(is.null(inChecker(c(input$sm_var)))) return(ret_text) not sure

		# return("nothing to see")
	data <- getdata()[,input$sum_var,drop=FALSE]
	id<-match("none",input$sum_group)
	if(is.na(id)){
		factor<-getdata()[,input$sum_group,drop=FALSE]
	} else {
		indata<-input$sum_group[-id]
		factor<-data.frame(id=rep("id",nrow(getdata())),getdata()[,indata,drop=FALSE])
	}
	formula<-paste(colnames(factor),collapse="*")
	sig.figs<-3
	test.obj<-join.columns(factor)

	#split data
	tmp<-sub.data(data,test.obj)
	fct<-factor(as.character(unlist(tmp[1]))) # breaks ordered factors
	tmp.data<-data.frame(tmp[[2]])

	# get stats
	res<-list()
	if(input$sum_formatresults=="separate"){
		if("sum_mean"%in%input$sum_stat_vars){res <-c(res,calc.stat(tmp.data,factor=fct,stat=c("mean"),na.rm=TRUE))}
		if("sum_median"%in%input$sum_stat_vars){res<-c(res,calc.stat(tmp.data,factor=fct,stat=c("median"),na.rm=TRUE))}
		if("sum_sd"%in%input$sum_stat_vars){res<-c(res,calc.stat(tmp.data,factor=fct,stat=c("sd"),na.rm=TRUE))}
		if("sum_min"%in%input$sum_stat_vars){res<-c(res,calc.stat(tmp.data,factor=fct,stat=c("min"),na.rm=TRUE))}
		if("sum_max"%in%input$sum_stat_vars){res<-c(res,calc.stat(tmp.data,factor=fct,stat=c("max"),na.rm=TRUE))}
		if("rsd"%in%input$sum_stat_vars){res<-c(res,calc.rsd(tmp.data,factor=fct))}
		if("FC"%in%input$sum_stat_vars){res<-c(res,calc.FC(tmp.data,factor=fct,denom=levels(fct)[1],sig.figs=3))}
		if("sum"%in%input$sum_stat_vars){res<-c(res,calc.stat(tmp.data,factor=fct,stat=c("sum"),na.rm=TRUE))}
		res<-do.call("cbind",res)
		rownames(res)<-colnames(tmp.data)
	}
	
	if(input$sum_formatresults=="formatted"){
			means<-calc.stat(tmp.data,factor=fct,stat=c("mean"),na.rm=TRUE)
			sds<-calc.stat(tmp.data,factor=fct,stat=c("sd"),na.rm=TRUE)
			medians<-calc.stat(tmp.data,factor=fct,stat=c("median"),na.rm=TRUE)
			mins<-calc.stat(tmp.data,factor=fct,stat=c("min"),na.rm=TRUE)
			maxs<-calc.stat(tmp.data,factor=fct,stat=c("max"),na.rm=TRUE)
			
			#parametric summary
			names<-paste(unlist(as.data.frame(strsplit(colnames(means),"-"))[2,])," mean +/- std dev" , sep="")
			mean.sd<-matrix(paste(unlist(signif(means,sig.figs)), " +/- ", unlist(signif(sds,sig.figs-1)),sep=""), ncol=ncol(means))
			colnames(mean.sd)<-names
			
			#non-parametric summary
			names<-paste(unlist(as.data.frame(strsplit(colnames(medians),"-"))[2,]),"-median (min, max)" , sep="")
			med.range<-matrix(paste(unlist(signif(medians,sig.figs)), " (", unlist(signif(mins,sig.figs)),", ",unlist(signif(maxs,sig.figs)),")",sep=""), ncol=ncol(means))
			colnames(med.range)<-names
			
			res<-cbind(mean.sd,med.range)
			names<-colnames(res)
			res<-data.frame(mean.sd,med.range)
			colnames(res)<-names
			rownames(res)<-colnames(tmp.data)
	}
	values$stats_summary<-data.frame(res)	
	return(res)
	
})

#save results
observe({ 
	if(is.null(input$save_stats_summary_results) || input$save_stats_summary_results == 0) return()
	isolate({
		name<-paste0(input$datasets,"_stats_summary")
		values[[name]]<-values$stats_summary
		values$datasetlist <- unique(c(values$datasetlist,name))
	})
})	

###############################
# t-Test
###############################
output$ttest_groups<-renderUI({
	vars <- varnames()
	isNum <- !"numeric" == getdata_class() & !"integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  #make sure vars are two level factors
  check<-getdata()[,vars,drop=FALSE]
  keep<-sapply(1:ncol(check),function(i){length(unique(check[,i]))==2})
  vars<-vars[keep]
  selectInput(inputId = "ttest_groups", label = "Grouping variables:", choices = vars, selected = names(vars), multiple = FALSE)
})

output$ttest_variables <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "ttest_var", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE, selectize=FALSE)
})

ui_ttest <- function() {
  list(
  	wellPanel(
		conditionalPanel(condition = "input.analysistabs != 'Plots'",
			uiOutput("ttest_groups"), # groups
			uiOutput("ttest_variables"), # variables
			selectInput("ttest_type", label = "test type", choices = c("one-sample"="onesample","two-sample"="twosample","paired"="paired"), selected = "twosample", multiple = FALSE),
			conditionalPanel(condition = "input.ttest_type == 'onesample'",
					numericInput("ttest_mu", "Comparison value:", 0.5, min = 0, max=1e23, step = 1)
			),
			selectInput("ttest_FDR", label = "adjusted p-value", choices = c(p.adjust.methods), selected = "BH", multiple = FALSE),
			selectInput("ttest_qvalue", label = "q-value", choices = c("Storey"="storey","FDRtool"="fdrtools"), selected = "FDRtool", multiple = FALSE),
			checkboxInput(inputId="ttest_take_log", label="Log transform", value = TRUE)
		),	
		conditionalPanel(condition = "input.analysistabs == 'Plots'",	
			selectInput("ttest_plot_pvalue_type", "Statistic:",choices=c("p-value"="p.val","adjusted p-value"="adj.p.val","q-value"="q.val"), selected="adjusted p-value",multiple=FALSE),
			numericInput("ttest_plot_FC_limit", "Fold Change cut off:", 2, min = 0, step = 0.25),	
			numericInput("ttest_plot_pvalue_limit", "P-value cut off:", .05, min = 0, max=1, step = 0.05),
			div(class="row-fluid", # see css.style for this as well
				div(class="span6",numericInput("ttest_plot_x_offset", "Label x offset:", value=.01, step=.01)),
				div(class="span6", numericInput("ttest_plot_y_offset", "Label y offset:", value=.4, step=.1))
			),
			tags$style(type="text/css", "#ttest_plot_x_offset {width:75px;}"),
			tags$style(type="text/css", "#ttest_plot_y_offset {width:75px;}"),
			numericInput("ttest_plot_size", "Size:", 4, min = 0, step=1),
			sliderInput(inputId="ttest_plot_angle", label="Text angle:", min=0, max=360, value=45, step = 10),
			numericInput("ttest_plot_alpha", "Transparency:", .5, min = 0, max=1, step=.1)
		),
		actionButton("save_t_test_results", "Save results")		
  	),
 		# helpModal('Single mean','singleMean',includeMarkdown("tools/help/singleMean.md"))
 		helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))
 	)
}

summary.ttest <- function(result) {
	result
}

plot.ttest <- function(result) {
	if(is.null(values$t_test_results)) return(empty.plot("Please carry out test first."))
	if(is.null(input$ttest_plot_y_offset)|is.null(input$ttest_plot_x_offset)) return(empty.plot(""))
	if(is.na(input$ttest_plot_y_offset)|is.na(input$ttest_plot_x_offset)) return(empty.plot("Need finite inputs.")) # tryCatch doesn't handle NA below!
	
	#data objects
	tmp<-values$t_test_results
	
	#set graph input
	p.value<-switch(input$ttest_plot_pvalue_type,
			'p.val' 		= tmp[,1],
			'adj.p.val' 	= tmp[,2],
			'q.val' 		= tmp[,3]
	)
	
	FC<-tmp[,4]
	
	#create plot
	tryCatch(volcano.plot(FC=FC,p.value=p.value,labels=rownames(tmp),FC.lim=input$ttest_plot_FC_limit,
		p.value.lim=input$ttest_plot_pvalue_limit,size=input$ttest_plot_size,alpha=input$ttest_plot_alpha,
		x.offset=input$ttest_plot_x_offset,y.offset=input$ttest_plot_y_offset,text.angle=input$ttest_plot_angle),error=function(e) {empty.plot("")})
	
}

ttest <- reactive({

	ret_text <- "This analysis requires the variables to be of type numeric or integer and group of type factor with two levels. Please select another dataset."
	if(is.null(input$ttest_groups)) return(ret_text)
	if(is.null(input$ttest_var)) return(ret_text)
	# # if(is.null(inChecker(c(input$sm_var)))) return(ret_text) not sure

		# return("nothing to see")
	data <- getdata()[,input$ttest_var,drop=FALSE]
	if(input$ttest_take_log) test.data<-log(data+1) else test.data<-data
	factor<-getdata()[,input$ttest_groups,drop=FALSE]
	formula<-paste(colnames(factor),collapse="*")
	
	if(input$ttest_type=="paired"){paired<-TRUE }else{ paired<-FALSE}
	if(input$ttest_type=="onesample"){mu<-input$ttest_mu }else{ mu<-NULL}
	# list(factor,mu,paired)
	ttest.results<-suppressMessages(multi.t.test(data=test.data, factor=factor,mu=mu,paired=paired,progress=FALSE,FDR=input$ttest_FDR,qvalue=input$ttest_qvalue)) # can't suppress printing of errors
	#calculate fold change
	
	FC<-tryCatch(calc.FC(data=data,factor=factor[,1],denom=unique(fixlc(factor[,1]))[1],sig.figs=1,log=FALSE),error=function(e){NA})
	values$t_test_results<-data.frame(ttest.results,FC)
	return(data.frame(ttest.results,FC))
	
})

#save results
observe({ 
	if(is.null(input$save_t_test_results) || input$save_t_test_results == 0) return()
	isolate({
		name<-paste0(input$datasets,"_t_test")
		values[[name]]<-values$t_test_results
		values$datasetlist <- unique(c(values$datasetlist,name))
	})
})

###############################
# ANOVA
###############################
output$anova_groups<-renderUI({
vars <- varnames()
	isNum <- !"numeric" == getdata_class() & !"integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
 
  selectInput(inputId = "anova_groups", label = "Test factors:", choices = vars, selected = names(vars), multiple = TRUE)
})

output$anova_groups_repeated<-renderUI({
vars <- varnames()
	isNum <- !"numeric" == getdata_class() & !"integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
 
  selectInput(inputId = "anova_groups_repeated", label = "Repeated measures:", choices = c("none",vars),  multiple = FALSE)
})

output$anova_variables <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "anova_var", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE, selectize=FALSE)
})

ui_anova <- function() {
  list(
  	wellPanel(
 	   	uiOutput("anova_groups"), # groups
		uiOutput("anova_groups_repeated"),
		checkboxInput("anova_interaction", label = "Interaction", value =TRUE),
		checkboxInput(inputId="anova_take_log", label="Log transform", value = TRUE),
		uiOutput("anova_variables"), # variables
		tags$style(type='text/css', "#anova_var { height: 150px}"),
		br(),
		selectInput("anova_FDR", label = "adjusted p-value", choices = c(p.adjust.methods), selected = "BH", multiple = FALSE),
		selectInput("anova_qvalue", label = "q-value", choices = c("Storey"="storey","FDRtool"="fdrtools"), selected = "FDRtool", multiple = FALSE),
		checkboxInput(inputId="anova_post_hoc", label="Post hoc", value = FALSE),
		br(),
		actionButton("save_anova_results", "Save results")	
  	),
 		# helpModal('Single mean','singleMean',includeMarkdown("tools/help/singleMean.md"))
 		helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))
 	)
}

summary.anova <- function(result) {
	result
}

plot.anova <- function(result) {
	empty.plot("Coming soon.")
}

anova <- reactive({

	ret_text <- "This analysis requires the variables to be of type numeric or integer and group of type factor. Please select another dataset."
	if(is.null(input$anova_groups)) return(ret_text)
	if(is.null(input$anova_var)) return(ret_text)
	# # if(is.null(inChecker(c(input$sm_var)))) return(ret_text) not sure

	# return("nothing to see")
	data <- getdata()[,input$anova_var,drop=FALSE]
	factor<-getdata()[,input$anova_groups,drop=FALSE]
	if(input$anova_interaction==TRUE){formula<-paste(colnames(factor),collapse="*")} else {formula<-paste(colnames(factor),collapse="+")}
	if(input$anova_take_log) test.data<-log(data+1) else test.data<-data
	#repeated measures
	if(input$anova_groups_repeated=="none" | is.null(input$anova_groups_repeated)) { 
			repeated<- NULL 
		} else { 
			repeated<- input$anova_groups_repeated
			factor[,repeated]<-getdata()[,input$anova_groups_repeated,drop=FALSE]
		}	
	
	#carry out tests
	test.res<-aov.formula.list(data=test.data,formula,meta.data=factor,post.hoc=input$anova_post_hoc,repeated=repeated,p.adjust=input$anova_FDR)
	
	# p.vals<-anova.formula.list(test.data,formula,meta.data=factor)
	p.vals<-test.res$p.values

	#multiple hypotheses tested adjustments	
	adj.p<-do.call("cbind",sapply(1:ncol(as.matrix(p.vals)),function(i)
		{
			as.data.frame(p.adjust(as.matrix(p.vals[,i]), method = input$anova_FDR, n = nrow(p.vals)))
		}))
	colnames(adj.p)<-paste(colnames(p.vals),"adjusted.p.values",sep="_")	
	#estimate q-values	
	if(input$anova_qvalue=="fdrtools"){
		adjusted.q<-do.call("cbind",lapply(1:ncol(as.matrix(p.vals)),function(i)
			{
				suppressMessages(FDR.adjust(as.matrix(p.vals[,i]),type="pvalue",return.all=TRUE)$qval)
			}))
	} else {
	adjusted.q<-do.call("cbind",lapply(1:ncol(as.matrix(p.vals)),function(i)
			{	
				suppressMessages(tryCatch(qvalue(as.matrix(p.vals[,i]))$qvalue,error=function(e){rep(1,length(p.vals[,i]))}))
			}))
	}
	
	colnames(p.vals)<-paste(colnames(p.vals),"p.values",sep="_")
	colnames(adjusted.q)<-gsub("adjusted.p.values","q.values",colnames(adj.p))		
	anova.results<-cbind(p.vals,adj.p,adjusted.q)
	values$anova_results<-list(p.values = anova.results,post.hoc=test.res$post.hoc)
	return(list(p.values = anova.results,post.hoc=test.res$post.hoc))
})

#save results
observe({ 
	if(is.null(input$save_anova_results) || input$save_anova_results == 0) return()
	isolate({
		name<-paste0(input$datasets,"_ANOVA")
		values[[name]]<-tryCatch(data.frame(do.call("cbind",values$anova_results)),error=function(e) {values$anova_results[[1]]}) # takes care of empty post hoc
		values$datasetlist <- unique(c(values$datasetlist,name))
	})
})