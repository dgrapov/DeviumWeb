###############################
# PLS/O-PLS
###############################

#convert vector to named list (may not need to do this)
namel<-function (vec){
		tmp<-as.list(vec)
		names(tmp)<-as.character(unlist(vec))
		tmp
	}

#UI
output$opls_variables <- renderUI({
  vars <- varnames()
	# isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	# vars <- vars[isNum]	
  if(length(vars) == 0) return()
  if(!is.null(input$opls_y_var)){vars<-vars[!vars%in%input$opls_y_var]}
  selectInput(inputId = "opls_var", label = "X variables:", choices = vars, selected = names(vars), multiple = TRUE, selectize=FALSE)
})

output$opls_yvariables<-renderUI({
  vars <- varnames()
  selectInput(inputId = "opls_y_var", label = "Y variables:", choices = vars, multiple = FALSE)
})

#observe Y and set defaults for OLPS discriminant analysis
observe({
	if(is.null(input$opls_y_var)) return()
	isolate({
		#detect if discriminant analysis
		#for now limit to 2 group comparisons
		tmp<-unique(fixlc(getdata()[,input$opls_y_var]))
		values$is.OPLSDA<-if(length(tmp)==2) TRUE else FALSE
	})

})

output$opls_trainindex<-renderUI({
  vars <- varnames()
  selectInput(inputId = "opls_test_train_var", label = "Test/Train index:", choices = vars, multiple = FALSE)
})

output$opls_groups<-renderUI({
vars <- varnames()
	isNum <- !"numeric" == getdata_class() & !"integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
 
  selectInput(inputId = "opls_groups", label = "Visualization variables:", choices = vars, multiple = TRUE)
})

output$opls_pcs<-renderUI({
		maxPCs<-ncol(getdata())
		numericInput("opls_pcs", "Latent variables", value=2, min = 2, max = maxPCs)
	})
	
output$opls_opcs<-renderUI({
		
		maxPCs<-ncol(getdata())
		numericInput("opls_o_pcs", "Orthogonal latent variables", value=1, min = 0, max = maxPCs)
	})	
		
output$opls_x_axis<-renderUI({
		numericInput("opls_x_axis", "x-axis", value=1, min = 1, max = input$opls_pcs)
	})
	
output$opls_y_axis<-renderUI({
		numericInput("opls_y_axis", "y-axis", value=2, min = 1, max = input$opls_pcs)
	})	
	
ui_opls <- function() {
  list(
  	wellPanel(
		conditionalPanel(condition = "input.analysistabs=='Plots'",downloadButton('Download_opls_plot', label = "Download plot")),
		br(),
		h3('Calculate'),
		tags$details(tags$summary("Model"),
		uiOutput("opls_yvariables"), # Y works best for single y
		uiOutput("opls_variables"), # variables
		uiOutput("opls_pcs"),
		uiOutput("opls_opcs"),
		checkboxInput("opls_center","Center",TRUE),
		selectInput("opls_scaling","Scale", list(none = "none", "unit variance" = "uv", pareto = "pareto"),selected="unit variance"),
		selectInput("opls_method","Method", list("NIPALS"= "oscorespls","kernel" = "kernelpls", "wide kernel"= "widekernelpls")),	# need to figure which are loading weights for "SIMPLS"="simpls"
		selectInput("opls_validation","Cross-validation", list(none = "none", "Leave-one-out"="LOO", "n-fold" = "CV"),selected="Leave-one-out")
		),
		tags$details(tags$summary("Validate"),
			numericInput("opls_n_tests", "Number of tests:", value=0, min = 0,step=10),
			checkboxInput("opls_permutation","Permutation testing?",TRUE),
			checkboxInput("opls_internal_testing","Internal training/testing?",TRUE),
			conditionalPanel(condition = "input.opls_internal_testing",
				selectInput("opls_internal_testing_var","Training/testing index",as.list(c("random",varnames())),selected="random")
			),
			checkboxInput("opls_external_testing","External training/testing?",TRUE),
			conditionalPanel(condition = "input.opls_external_testing",
				selectInput("opls_external_testing_var","Training/testing index",as.list(c("random","duplex",varnames())),selected="duplex")
			)
		),
		tags$details(tags$summary("Select Features"),
			# checkboxInput("opls_feature_selection","Identify top predictors?",FALSE),
			#options
			selectInput("opls_feature_selection_type","Selection type:",list(none="none",quantile = "quantile",number = "number"),selected="none",multiple=FALSE),
			numericInput("opls_feature_selection_type_value", "Top percent", value=10, min = 0,max=100,step=5),
			# conditionalPanel(condition = "input.opls_feature_selection_type == 'quantile'",
				# numericInput("opls_feature_selection_type_value_quant", "Top percent", value=10, min = 0,max=100,step=5)
			# ),
			# conditionalPanel(condition = "input.opls_feature_selection_type == 'number'",
				# numericInput("opls_feature_selection_type_value_rank", "Top number", value=2, min = 1,step=1)
			# ),
			# h5('Correlation with Scores'),
			conditionalPanel(condition = "input.opls_feature_selection_type != 'none'",
			selectInput("opls_feature_selection_feature_weight","Weight:",list("loading","coefficient","VIP"),selected="loading",multiple=FALSE),
			selectInput("opls_feature_selection_cor_type","Type:",list("spearman","pearson","biweight"),selected="spearman",multiple=FALSE),
			numericInput("opls_feature_selection_p_val", "p-value:", value=0.05, min = 0,max=1,step=0.005),
			checkboxInput("opls_feature_selection_FDR","FDR",TRUE),
			checkboxInput("opls_feature_selection_separate","separate signs",FALSE)
			)
		),
		h3('Plot'),
			tags$details(tags$summary("Options"),	
				selectInput("opls_plot","Plot type",
					list ("RMSEP" 	= "RMSEP",
					"Scores" 		= "scores", 
					"Loadings" 		= "loadings",
					"Biplot" 		= "biplot",
					"multi-RMSEP" 	= "osc.RMSEP",
					"multi-Scores" 	= "osc.scores"),selected="RMSEP"),
					# "multi-Loadings"= "osc.loadings",
					# "multi-Weights" = "osc.delta.weights"),
				conditionalPanel(condition = "input.opls_plot == 'scores'|input.opls_plot == 'biplot'|input.opls_plot == 'osc.scores'|input.opls_plot == 'loadings'",#condition = "input.opls_plot != 'RMSEP'",
							tags$details(open="open",tags$summary("Groups"),
								conditionalPanel(condition = "input.opls_plot != 'loadings'",
									uiOutput("opls_groups")
								),
								selectInput("opls_group_bounds","Boundary:",list (none = "none", ellipse =  "ellipse",polygon = "polygon"),selected="ellipse"),
								conditionalPanel(condition = "input.opls_group_bounds != 'none'",
									numericInput("opls_group_alpha", "Transparency:",value=.35, min = 0,max=1,step=.1)
								)
							),
						tags$details(tags$summary("Components"),
						# h5('Components'),
							uiOutput("opls_x_axis"),
							uiOutput("opls_y_axis")
						),
						tags$details(tags$summary("Points"),
						# h5('Points'),
							numericInput("opls_size", "Size:", value=5, min = 0,step=1),
							numericInput("opls_point_alpha", "Transparency", value=.75, min = 0,max=1,step=.1)
						),
						tags$details(tags$summary("Labels"),
						# h5('Labels'),
							checkboxInput("opls_show_labels","Show",TRUE),
							numericInput("opls_label_font_size", "Size:", value=5, min = 0,step=1),
							textInput("opls_legend_label", "Legend title", value = "auto")	
						)		
				)
			),	
		h3('Save'),
			tags$details(tags$summary("Objects"),
				selectInput("opls_result_obj","",choices=c("-----" = ""),selected = "-----",multiple=TRUE),#,values[["opls_objects"]]$return_obj_name),
				actionButton("save_opls_result_obj", "Save")
			)
		)
		# with modal included the header size tags (e.g. h4, h5) are not interpreted?
		# br(),
 		# helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))
 	)
}

#update plot options based on chosen inputs
observe({
	tmp<-list ("RMSEP" 	= "RMSEP",
					"Scores" 		= "scores", 
					"Loadings" 		= "loadings",
					"Biplot" 		= "biplot",
					"multi-RMSEP" 	= "osc.RMSEP",
					"multi-Scores" 	= "osc.scores")
	if(!is.null(input$opls_feature_selection_type))	{			
		if(!input$opls_feature_selection_type=="none")	{tmp<-c(tmp,"feature selection"="feature selection")}		
	}
	updateSelectInput(session, "opls_plot", choices=tmp,selected=tmp[1])
})	

summary.opls <- function(result) {
	result
}

opls <- reactive({
	
	if(is.null(input$datasets)) return()
	opls_result_objects<-list() # store names of values to return
	
	ret_text <- "This analysis requires >2 variables to be of type numeric or integer and no missing values in the data. Please select another dataset."
	if(is.null(input$opls_y_var)) return("Please select Y variable(s).")
	if(is.null(input$opls_var)) return(ret_text)
	# # if(is.null(inChecker(c(input$sm_var)))) return(ret_text) not sure
	
	# Y
	y<- getdata()[,input$opls_y_var,drop=FALSE]
	#check for factors and convert to numeric
	pls.y<-do.call("cbind",lapply(1:ncol(y),function(i){as.numeric(y[,i])})) # convert to numeric
	colnames(pls.y)<-colnames(y)
	#check for discriminant analysis
	is.OPLSDA<-if(is.null(values$is.OPLSDA)) FALSE else values$is.OPLSDA
	
	# X/DATA
	opls.data <- getdata()[,input$opls_var,drop=FALSE]
	names<-dimnames(opls.data)
	opls.data<-do.call("cbind",lapply(1:ncol(opls.data),function(i){as.numeric(opls.data[,i])})) # convert to numeric
	dimnames(opls.data)<-names
	scaled.data<-data.frame(prep(opls.data,center=input$opls_center,scale=input$opls_scaling))
	
	
	# group
	if(is.null(input$opls_groups)||input$opls_groups=="none"){opls_groups<-NULL } else {opls_groups<-input$opls_groups}
	opls.group<-getdata()[,opls_groups,drop=FALSE]
	# training testing index
	
	#should CV folds be normalized separately (only works for uv and centred)?
	if(input$opls_validation=="uv"&!input$opls_validation=="none"){cv.scale<-TRUE} else {cv.scale<-FALSE}
	
	#make model (should store results in a common object)
	opls.results<-values$opls.results<-make.OSC.PLS.model(pls.y,pls.data=scaled.data,
						comp=input$opls_pcs,
						OSC.comp=input$opls_o_pcs, 
						validation = input$opls_validation, 
						method=input$opls_method, 
						cv.scale=cv.scale, 
						train.test.index=NULL,
						OPLSDA=is.OPLSDA,
						progress=FALSE)					
	final.opls.results<-values$final.opls.results<-get.OSC.model(obj=values$opls.results,OSC.comp=input$opls_o_pcs)		
	#report basic model stats
	yvar.mnames<-paste0(paste0("(",1:length(input$opls_y_var),")"),input$opls_y_var,collapse=" ")
	mod.description<-data.frame(selection = t(data.frame(yvar.mnames,input$opls_pcs,
						input$opls_o_pcs, 
						input$opls_validation, 
						input$opls_method)))

					
						
	rownames(mod.description)<-c("Dependent Variables","Latent variables (LVs)","Orthogonal latent variables (OLVs)","model cross-validation", "method")				
	opls.model.text<-data.frame("Xvar"=c(0,round(cumsum(values$final.opls.results$Xvar)*100,2)),"Q2"=values$final.opls.results$Q2,"RMSEP"= values$final.opls.results$RMSEP)	
	rownames(opls.model.text)<-c("intercept",paste0("LV ",1:input$opls_pcs))
	#OPLSDA text
	OPLSDA.text<-if(is.OPLSDA) data.frame(values$final.opls.results$OPLSDA.stats) else NULL
		
	#feature selection
	#---------------------------
	# if(input$opls_feature_selection){
	if(!input$opls_feature_selection_type == "none"){
	#modify inputs based on GUI

	if(input$opls_feature_selection_type=="quantile"){
			top<-1-input$opls_feature_selection_type_value/100
			# if(input$opls_feature_selection_separate){top<-top/2}
			# return.max<-ceiling(ncol(scaled.data)*top)
		} else {
			top<-input$opls_feature_selection_type_value # c
			# if(input$opls_feature_selection_separate){top<-ceiling(top/2)}
			if(top>ncol(scaled.data)) {top<-ncol(scaled.data)}
			# return.max<-top
		}
		
		.scores<-values$final.opls.results$scores[,]
		.loadings<-switch(input$opls_feature_selection_feature_weight,
							"loading"=values$final.opls.results$loadings[,][,1],
							"coefficient" = values$final.opls.results$coefficients[,ncol(values$final.opls.results$coefficients)],
							"VIP" = matrix(values$final.opls.results$VIP)[,1]) 
		selected.features<-values$opls.selected.features<-PLS.feature.select(pls.data=scaled.data,pls.scores=.scores[,1],pls.loadings=.loadings,pls.weight=.loadings,
					p.value=input$opls_feature_selection_p_val, FDR=input$opls_feature_selection_FDR,
					cut.type=input$opls_feature_selection_type,top=top,
					separate=input$opls_feature_selection_separate,type=input$opls_feature_selection_cor_type,plot=FALSE)
		
		# #store results for later
		name<-paste0(input$datasets,"_opls_selection_info")	
		values[[name]]<-data.frame(index=1:ncol(opls.data),selected.features)
		opls_result_objects<-c(opls_result_objects,name)
		
		# values[["opls_objects"]]$opls_features_object_name<-name	
		# values[["opls_objects"]]$opls_selected_features_report<-data.frame(index=1:ncol(opls.data),selected.features)
		# values[["opls_objects"]]$opls_selected_features_report_name<-name<-paste0(input$datasets,"_OPLS_feature_info")					
		# values[["opls_objects"]]$return_obj_name<-c(values[["opls_objects"]]$return_obj_name,paste0(input$datasets,"_OPLS_feature_info"))
		
		#method description			
		feat.decription<-data.frame(options=t(feat.decription<-data.frame(p.value=input$opls_feature_selection_p_val, FDR=input$opls_feature_selection_FDR,
					cut.type=input$opls_feature_selection_type,top=top,
					separate=input$opls_feature_selection_separate,type=input$opls_feature_selection_cor_type)))
					
		#selected objects
		tmp<-selected.features[selected.features$combined.selection==TRUE,1,drop=FALSE] # weight
		# tmp<-tmp[order(abs(tmp$loadings),decreasing=TRUE),,drop=FALSE][1:return.max,,drop=FALSE]		
		summary.selected.features<-list(tmp,feat.decription)
		
		#need a mechanism to save feature selected data as a separate data set
		#this should be trigged by a button
		# opls.sel.data<-cbind(pls.y,getdata()[,rownames(tmp),drop=FALSE])# better leave y alone!!!!!
		opls.sel.data<-cbind(pls.y,getdata()[,rownames(tmp),drop=FALSE])
		
		# isolate({
			# # name<-paste0(input$datasets,"_opls_features")
			# # values$datasetlist <- unique(c(values$datasetlist,name) )
			# # values[[name]]<-opls.sel.data
			
		# #store object for later save
		name<-paste0(input$datasets,"_opls_features")
		values[["opls_objects"]]$opls_features_object_name<-name
		values[[name]]<-opls.sel.data
		opls_result_objects<-c(opls_result_objects,name)
		
		
	} else {
		summary.selected.features<-NULL
		values$opls.selected.features<-NULL
	}
	
	#external training/testing model cross validation
	ext.test.train.results<-NULL
	#need to split data in advance
	
	
	# #internal training/testing cross validation
	if(input$opls_n_tests>0&input$opls_internal_testing==TRUE){
		ntests<-input$opls_n_tests
		train.id<-input$opls_internal_testing_var
		if(train.id=="duplex"|train.id=="random"){
			#sampling from groups
			strata.var<-getdata()[,input$opls_y_var,drop=FALSE]
			strata<-if(all(is.factor(strata.var))){
				strata<-join.columns(strata.var)
				tmp<-rbind(as.matrix(mod.description),'Internal CV stratification:'=paste(colnames(strata.var),collapse="|"))
				mod.description<-data.frame(tmp)
			} else {
				strata<-NULL
			} 
				int.train.test.index <- test.train.split(nrow(scaled.data), n = ntests, strata = strata, split.type = train.id, data = scaled.data)	
				tmp<-rbind(as.matrix(mod.description),"Internal train/test index"=paste0(ntests," repetitions ","generated by ",train.id))
				mod.description<-data.frame(tmp)
				
		} else {
				#user specified train/test need to be "train" and "test" (add 1, 0 where 1=test)
				int.train.test.index<-getdata()[,input$opls_internal_testing_var,drop=FALSE]
				if(!length(c("test","train")%in%unique(int.train.test.index))==2)
				int.train.test.index<-NULL
				tmp<-rbind(as.matrix(mod.description),"Internal train/test index" = "Custom input needs top be 'test' or 'train'")
				mod.description<-data.frame(tmp)
			}
	} else {int.train.test.index<-NULL}
			
	#permutation testing (may use int.test.train.index) #TODO: switch to updated function
	if(input$opls_n_tests>0&input$opls_permutation==TRUE){
		permutation.results<-values$opls.permutation.results<-permute.OSC.PLS(data = scaled.data, y = pls.y, 
			n = input$opls_n_tests, ncomp = input$opls_pcs, 
			osc.comp = input$opls_o_pcs, 
			train.test.index = int.train.test.index,
			OPLSDA=is.OPLSDA,
			progress = FALSE)
	} else {
		permutation.results<-NULL
	}	
	
	#carry out training/testing	and or permutations (limiting to first Y)
	if(input$opls_n_tests>0&input$opls_internal_testing==TRUE){
			int.test.train.results<-values$int.test.train.results<-OSC.PLS.train.test(pls.data = scaled.data, pls.y = pls.y[,1,drop=FALSE], train.test.index=int.train.test.index,
									comp = input$opls_pcs, 
									OSC.comp = input$opls_o_pcs, 
									cv.scale = cv.scale,
									validation = input$opls_validation, 
									method=input$opls_method,
									OPLSDA=is.OPLSDA,
									progress = FALSE)
							
			model.performance<-OSC.validate.model(model = values$final.opls.results, perm = permutation.results, train = int.test.train.results,test="perm.test2")
			int.test.perf<-model.performance
		} else {
			
			model.performance<-OSC.validate.model(model = values$final.opls.results, perm = permutation.results, train = NULL,test="perm.test2")
			int.test.perf<-model.performance
		}
	
	# #carry out validations on feature selected object
	# if(exists("opls.sel.data")){
		# sel.scaled.data<-scaled.data<-data.frame(prep(opls.sel.data,center=input$opls_center,scale=input$opls_scaling))		
		# #permutation testing (may use int.test.train.index)
		# if(input$opls_n_tests>0&input$opls_permutation==TRUE){
			# sel.permutation.results<-values$sel.opls.permutation.results<-permute.OSC.PLS(data = sel.scaled.data, y = pls.y, 
				# n = input$opls_n_tests, ncomp = input$opls_pcs, 
				# osc.comp = input$opls_o_pcs, 
				# train.test.index = int.train.test.index,
				# progress = FALSE)
		# } else {
			# sel.permutation.results<-NULL
		# }	
		
		# #carry out training/testing	and or permutations (limiting to first Y)
		# if(input$opls_n_tests>0&input$opls_internal_testing==TRUE){
			# sel.model<-make.OSC.PLS.model(pls.y,pls.data=sel.scaled.data,
							# comp=input$opls_pcs,
							# OSC.comp=input$opls_o_pcs, 
							# validation = input$opls_validation, 
							# method=input$opls_method, 
							# cv.scale=cv.scale, 
							# train.test.index=NULL,
							# progress=FALSE)				
			# sel.int.test.train.results<-values$sel.int.test.train.results<-OSC.PLS.train.test(pls.data = sel.scaled.data, pls.y = pls.y[,1,drop=FALSE], train.test.index=int.train.test.index,
									# comp = input$opls_pcs, 
									# OSC.comp = input$opls_o_pcs, 
									# cv.scale = cv.scale,
									# validation = input$opls_validation, 
									# method=input$opls_method,
									# progress = FALSE)
			# sel.model.performance<-OSC.validate.model(model = sel.model, perm = sel.permutation.results, train = sel.int.test.train.results)
			# sel.int.test.perf<-sel.model.performance
		# } else {
			# sel.model<-make.OSC.PLS.model(pls.y,pls.data=sel.scaled.data,
							# comp=input$opls_pcs,
							# OSC.comp=input$opls_o_pcs, 
							# validation = input$opls_validation, 
							# method=input$opls_method, 
							# cv.scale=cv.scale, 
							# train.test.index=NULL,
							# progress=FALSE)			
			# sel.model.performance<-OSC.validate.model(model = sel.model, perm = sel.permutation.results, train = NULL)
			# sel.int.test.perf<-sel.model.performance
		# }
		
	# }
	
	
	# #--------------------------------------------
	# #scores, fitted values, residual, etc
	name<-paste0(input$datasets,"_OPLS_sample_info")
	# #add index, and Y for visualizations
	info<-data.frame(index=c(1:nrow(values$final.opls.results$y[[1]])),values$final.opls.results$y[[1]])
	tmp.obj<-data.frame(info,scores=values$final.opls.results$scores[,],fitted.values=values$final.opls.results$fitted.values[,],residuals=values$final.opls.results$residuals[,])
	#add meta data and rownames
	meta<-fixlr(getdata(),.remove=F)
	tmp.obj<-data.frame(names=rownames(values$final.opls.results$scores),meta,tmp.obj)
	values[["opls_objects"]]$opls_sample_info_name<-name
	values[[name]]<-tmp.obj	
	opls_result_objects<-c(opls_result_objects,name)
	
	# #loadings, coefficients, VIP, etc
	name<-paste0(input$datasets,"_OPLS_variable_info")
	tmp.obj<-data.frame(index=c(1:nrow(values$final.opls.results$loadings)),names=rownames(values$final.opls.results$loadings),loadings=values$final.opls.results$loadings[,],coefficients=values$final.opls.results$coefficients)
	tmp.obj$VIP<-values$final.opls.results$VIP[,] # may not exist
	tmp.obj$names<-rownames(values$final.opls.results$loadings)
	values[["opls_objects"]]$opls_variable_info_name<-name
	values[[name]]<-tmp.obj	
	opls_result_objects<-c(opls_result_objects,name)
	
	values$opls_result_objects<-unlist(unique(opls_result_objects)) #all results
	return(list(description=mod.description,statistics = opls.model.text,OPLSDA=OPLSDA.text,"Validated Model Performance (Y1)"=int.test.perf,selected.features=summary.selected.features))
	
})

#save results
observe({
	
	#meta data for scores
	if(is.null(input$datasets)) return()

	# #update objects to save
	# isolate({
		# if(!is.null(values[["opls_objects"]]$opls_selected_features_object)){
			# #store object for later save
			# name<-paste0(input$datasets,"_opls_features")
			# values[["opls_objects"]]$opls_features_object_name<-name
			# values[[name]]<-values[["opls_objects"]]$opls_selected_features_object
			# values[["opls_objects"]]$return_obj_name<-c(values[["opls_objects"]]$return_obj_name,name)	
		# }
		
		# if(!is.null(values$final.opls.results$scores)){
			# name<-paste0(input$datasets,"_OPLS_sample_info")
			# # #add index, and Y for visualizations
			# info<-data.frame(index=c(1:nrow(values$final.opls.results$y[[1]])),values$final.opls.results$y[[1]])
			# tmp.obj<-data.frame(info,scores=values$final.opls.results$scores[,],fitted.values=values$final.opls.results$fitted.values[,],residuals=values$final.opls.results$residuals[,])
			# #add meta data and rownames
			# meta<-fixlr(getdata(),.remove=F)
			# tmp.obj<-data.frame(names=rownames(values$final.opls.results$scores),meta,tmp.obj)

			# # store for later use
			# values[["opls_objects"]]$opls_sample_info_name<-name
			# values[[name]]<-tmp.obj	
			# values[["opls_objects"]]$return_obj_name<-c(values[["opls_objects"]]$return_obj_name,name)	
		# }
		
		# if(!is.null(values$final.opls.results$loadings)){
			# #loadings, coefficients, VIP, etc
			# name<-paste0(input$datasets,"_OPLS_variable_info")
			# tmp.obj<-data.frame(index=c(1:nrow(values$final.opls.results$loadings)),names=rownames(values$final.opls.results$loadings),loadings=values$final.opls.results$loadings[,],coefficients=values$final.opls.results$coefficients)
			# tmp.obj$VIP<-values$final.opls.results$VIP[,] # may not exist
			# tmp.obj$names<-rownames(values$final.opls.results$loadings)
			
			# #store for later use
			# values[["opls_objects"]]$opls_variable_info_name<-name
			# values[[name]]<-tmp.obj	
			# values[["opls_objects"]]$return_obj_name<-unique(c(values[["opls_objects"]]$return_obj_name,name))	
			
		# }
	# })		
		
	#update GUI
	if(is.null(values$opls_result_objects)) return()
	updateSelectInput(session, "opls_result_obj", choices = c("-----" = "",values$opls_result_objects))
	
	# #save
	if(is.null(input$save_opls_result_obj) || input$save_opls_result_obj == 0) return()
	isolate({
		values$datasetlist <- unique(c(values$datasetlist,values$opls_result_objects))
	})	
})	


#change GUI based on inputs
#change input$opls_feature_selection_type_value_quant based on input$opls_feature_selection_type value
observe({
	if(is.null(input$opls_feature_selection_type)) return()
	if(input$opls_feature_selection_type=="number"){
		updateNumericInput(session,"opls_feature_selection_type_value", "Top number",min=1,step=5)
	}
	if(input$opls_feature_selection_type=="quantile"){
		updateNumericInput(session,"opls_feature_selection_type_value", "Top percent",min=0,max=100,step=5,value=10)
	}	
})


#plot results
plot.opls <- function(result) {
	#set colors (getting ugly!)
	opls_groups<-input$opls_groups
	if(is.null(opls_groups)){opls_groups<-"none"}
	if(!opls_groups=="none"){color<- getdata()[,opls_groups,drop=FALSE]} else {color<-NULL}
	if(input$opls_plot=="loadings"){ color<-NULL}
	if(!is.null(color)){
		cnames<-paste(colnames(color),collapse="|")	
		color<-data.frame(color=join.columns(color))
		colnames(color)<-cnames
	} 
	
	if(input$opls_plot=="feature selection"){ #
		opts<-values$opls.selected.features
		plot.S.plot(opts,return="all")
	} 
	
	#create non feature
	OSC.plots<-length(agrep("osc.",input$opls_plot))==1
	if(OSC.plots){ 
		opls_plot<-gsub("osc.","",input$opls_plot)
		plot.OSC.results(values$opls.results,plot=opls_plot,groups=color)
	} 
			
	if(input$opls_plot%in%c("RMSEP","scores","loadings","biplot")){
		if(input$opls_legend_label=="auto"){ legend.name=NULL }else {legend.name<-input$opls_legend_label }
		plot.PLS(obj=values$final.opls.results,xaxis=input$opls_x_axis,yaxis=input$opls_y_axis, results = input$opls_plot,
		group.bounds=input$opls_group_bounds,size=input$opls_size,color=color, label=input$opls_show_labels, 
		legend.name =  legend.name,font.size=input$opls_label_font_size,alpha=input$opls_point_alpha,g.alpha=input$opls_group_alpha)
	}		
	
}

#save plot
save.plot.opls.name<-reactive(function(){
	# paste(input$datasets,input$plot_type,sep="-")
	sprintf("%s_%s.%s",input$datasets,input$opls_plot,'pdf')
	})

output$Download_opls_plot<-downloadHandler(
      filename = save.plot.opls.name,
    # a function that actually opens a connection to a pdf and print the result to it
    content = function(FILE=NULL) {
	
		# make.ggplot()
		# ggsave(file="plot.pdf")#FILE
		
	    pdf(file=FILE, onefile=T, width=12,height=8)
		# print(make_PC_Plot())
		plot.opls()
		dev.off()
	}
)
