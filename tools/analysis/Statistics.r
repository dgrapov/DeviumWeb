###############################
# Data summary
###############################
#fix:
# output for 1 row, 
# control of formatted choices
# colnames export

output$summary_groups<-renderUI({
vars <- varnames()
	isNum <- !"numeric" == getdata_class() & !"integer" == getdata_class()|!"logical" == getdata_class()
 	vars <- vars[isNum]
  # if(length(vars) == 0) return()
  selectInput(inputId = "sum_group", label = "Grouping variables:", choices = c("none"="none",vars),selected="none", multiple = TRUE)
})

output$summary_variables <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class() | "logical" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "sum_var", label = "Data:", choices = vars, selected = names(vars), multiple = TRUE)
  # selectInput(inputId = "sum_var", label = "Data:", choices = vars,  multiple = TRUE)
})

ui_datasummary <- function() {
  list(
  	wellPanel(
		uiOutput("summary_variables"), # variables
		uiOutput("summary_groups"), # groups
		conditionalPanel(condition = "input.analysistabs == 'Summary'",
			selectInput(inputId = "sum_stat_vars", label = "Statistics", choices = list("mean"="sum_mean","median"="sum_median","sum"="sum","standard deviation"="sum_sd","minimum"="sum_min","maximum"="sum_max","RSD"="rsd","Fold change"="FC"), selected = c("mean","standard deviation"), multiple =TRUE),
			radioButtons(inputId = "sum_formatresults", label = "Format results:", c("Separate" = "separate", "Formatted" = "formatted"), selected = "Separate"),
			actionButton("save_stats_summary_results", "Save results")			
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
	
	factor<-getdata()[,input$sum_group,drop=FALSE]
	if(is.null(factor)){
		return("Select data with factors or create factors for the current data set.")
	}
	data <- getdata()[,input$sum_var,drop=FALSE]
	test.obj<-join.columns(factor)
	if(ncol(factor)>1){legend.name<-paste0(colnames(factor),collapse="|")} else {legend.name<-colnames(factor)}
	#create box plots
	plots <- list()
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
	dat<-data.frame(data,group=factor(test.obj, ordered=FALSE))
		
	for(i in 1:ncol(data)){
		plots[[i]]<-ggplot(dat, aes_string(x = "group", y = colnames(dat)[i],fill = "group")) + 
		geom_boxplot() +.theme + geom_point(color='black',alpha=.35, position = 'jitter')+ 
		ggtitle(colnames(dat)[i]) + ylab("")+xlab("")+scale_fill_discrete(name = legend.name)
	}
	if(ncol(data)>1){
		print(do.call(grid.arrange, c(plots, list(ncol = 2))))
	} else {print(plots)}
	
}

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
		indata<-values$AAindata<-input$sum_group[-id]
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
  check<-getdata()[,vars]
  keep<-sapply(1:ncol(check),function(i){length(unique(check[,i]))==2})
  vars<-vars[keep]
  
  selectInput(inputId = "ttest_groups", label = "Grouping variables:", choices = vars, selected = names(vars), multiple = FALSE)
})

output$ttest_variables <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "ttest_var", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE)
})

ui_ttest <- function() {
  list(
  	wellPanel(
 	   	uiOutput("ttest_groups"), # groups
		uiOutput("ttest_variables"), # variables
		selectInput("ttest_type", label = "test type", choices = c("one-sample"="onesample","two-sample"="twosample","paired"="paired"), selected = "twosample", multiple = FALSE),
		conditionalPanel(condition = "input.ttest_type == 'onesample'",
					numericInput("ttest_mu", "Comparison value:", 0.5, min = 0, max=1e23, step = 1)
			),
		selectInput("ttest_FDR", label = "adjusted p-value", choices = c(p.adjust.methods), selected = "BH", multiple = FALSE),
		selectInput("ttest_qvalue", label = "q-value", choices = c("Storey"="storey","FDRtool"="fdrtools"), selected = "FDRtool", multiple = FALSE),
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
	plot(1,type="n",main="Check back soon")
}

ttest <- reactive({

	ret_text <- "This analysis requires the variables to be of type numeric or integer and group of type factor with two levels. Please select another dataset."
	if(is.null(input$ttest_groups)) return(ret_text)
	if(is.null(input$ttest_var)) return(ret_text)
	# # if(is.null(inChecker(c(input$sm_var)))) return(ret_text) not sure

		# return("nothing to see")
	data <- getdata()[,input$ttest_var,drop=FALSE]
	factor<-getdata()[,input$ttest_groups,drop=FALSE]
	formula<-paste(colnames(factor),collapse="*")
	
	if(input$ttest_type=="paired"){paired<-TRUE }else{ paired<-FALSE}
	if(input$ttest_type=="onesample"){mu<-input$ttest_mu }else{ mu<-NULL}
	# list(factor,mu,paired)
	ttest.results<-suppressMessages(multi.t.test(data=data, factor=factor,mu=mu,paired=paired,progress=FALSE,FDR=input$ttest_FDR,qvalue=input$ttest_qvalue)) # can't suppress printing of errors
	values$t_test_results<-ttest.results
	return(ttest.results)
	
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
 
  selectInput(inputId = "anova_groups", label = "Grouping variables:", choices = vars, selected = names(vars), multiple = TRUE)
})

output$anova_variables <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "anova_var", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE)
})

ui_anova <- function() {
  list(
  	wellPanel(
 	   	uiOutput("anova_groups"), # groups
		uiOutput("anova_variables"), # variables
		checkboxInput("anova_interaction", label = "Interaction?", value =TRUE),
		selectInput("anova_FDR", label = "adjusted p-value", choices = c(p.adjust.methods), selected = "BH", multiple = FALSE),
		selectInput("anova_qvalue", label = "q-value", choices = c("Storey"="storey","FDRtool"="fdrtools"), selected = "FDRtool", multiple = FALSE),
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
	plot(1,type="n",main="Check back soon")
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

	p.vals<-anova.formula.list(data,formula,meta.data=factor)

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
				tryCatch(qvalue(as.matrix(p.vals[,i]))$qvalue,error=function(e){rep(1,length(p.vals[,i]))})
			}))
	}
	
	colnames(p.vals)<-paste(colnames(p.vals),"p.values",sep="_")
	colnames(adjusted.q)<-gsub("adjusted.p.values","q.values",colnames(adj.p))		
	anova.results<-cbind(p.vals,adj.p,adjusted.q)
	values$anova_results<-anova.results
	return(anova.results)
})

#save results
observe({ 
	if(is.null(input$save_anova_results) || input$save_anova_results == 0) return()
	isolate({
		name<-paste0(input$datasets,"_ANOVA")
		values[[name]]<-values$anova_results
		values$datasetlist <- unique(c(values$datasetlist,name))
	})
})