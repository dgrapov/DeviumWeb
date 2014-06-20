#power analysis fxns
# check.get.packages("pwr")

#anova and t-test power very different for 2 classes?
# n<-32
# effect<-.4
# alpha<-.05

# pwr.t.test(n = n, d = effect, sig.level = alpha, power = NULL, type = "two.sample")
# pwr.anova.test(f=effect,k=2,n=n,sig.level=alpha,power=NULL)

# pwr.t.test(n = n, d = effect, sig.level = NULL, power = .8, type = "two.sample")
# pwr.anova.test(f=effect,k=3,n=n,sig.level=NULL,power=.8)

# # # for debugging overwrites all summary outputs
# output$summary<- renderPrint({
	# poweranalysis()
	# obj<-names(input)
	# input.obj<-lapply(1:length(obj), function(i) { input[[obj[i]]]})
	# names(input.obj)<-obj
	# obj<-names(values)
	# values.obj<-lapply(1:length(obj), function(i) { values[[obj[i]]]})
	# names(values.obj)<-obj
	# return(list(input = input.obj,values = values.obj))
# })

summary.poweranalysis<- function(results){
	# trigger print of reactives
	poweranalysis()
}

# #test
# data<-data.frame(mtcars[,1:3])
# factor<-test.factor<-data.frame(mtcars$am)
# calc<-"power"
# val<-0.05
# power.analysis.from.data(data,factor, calc, val=val)

power.analysis.from.data<-function(data,factor,calc,val){

	#get summary of data
	# factor<-data.frame(factor=join.columns(row.metadata))# make everything a single level
	# formula<-paste(colnames(factor),collapse="*")
	# stats<-stats.summary(data,comp.obj=factor,formula=formula,sigfigs=3,log=FALSE)
	
	
	#get summary for calculating effect size
	pwr.summary<-function(x){
			c(mean= tryCatch(mean(x),error = function(e) {NA}),
			sd = tryCatch(sd(x),error = function(e) {NA}),
			n = tryCatch(length(x),error = function(e) {NA}))
		}

	tmp.data<-data.frame(factor=as.factor(factor[,1]),as.data.frame(data))
	out.put<- ddply(.data=tmp.data, .(factor), colwise(pwr.summary),.progress = "none")

	#generalize power calc to 2 group comparisons 
	results<-function(x,factor1,variable,factor2,calc,val)
				{
					tmp<-split(data.frame(x),factor1)
					var<-split(data.frame(variable), factor2)
					res<-do.call("rbind",lapply(1:length(tmp), function(i){
						obj1<-unlist(tmp[[i]])
						var1<-var[[i]]
						id<-names(tmp)[!1:length(tmp)== i]
						tmp2<-tmp[id]
						.var<-var[[id]]
						res2<-do.call("rbind",lapply(1:length(tmp2), function(j){
							obj2<-unlist(tmp2[[j]])
							var2<-.var[j]
							p.sd<-tryCatch((((obj1[3]-1)*obj1[2]^2 +(obj2[3]-1)*obj2[2]^2)/(obj1[3]+obj2[3]-2))^.5,error = function(e) {NA})
							delta.means<-tryCatch(abs(obj1[1]-obj2[1]),error = function(e) {NA})
							effect.size<-tryCatch(delta.means/p.sd,error = function(e) {NA})
							#p.value<-tryCatch(t.test(x=var1, y = var2)$p.value, error=function(e) {NA})
							if(calc =="power"){
								p.value<-val
								power<-tryCatch(pwr.t2n.test(n1 = obj1[3], n2= obj2[3], d = effect.size , sig.level = val, power = NULL)$power,error = function(e) {NA})
								c(paste0(names(tmp)[i], "-",names(tmp2)[j]),signif(effect.size,3),signif(p.value,4),signif(power,2))
							} else {
								power<-val
								p.value<-tryCatch(pwr.t2n.test(n1 = obj1[3], n2= obj2[3], d = effect.size , sig.level = NULL, power = val)$sig.level,error = function(e) {NA})
								c(paste0(names(tmp)[i], "-",names(tmp2)[j]),signif(effect.size,3),signif(p.value,4),signif(power,2))
							}
							}))		
					}))
					colnames(res)<-c("comparison","effect size","alpha","power")
					#keep unique
					tmp<-t(data.frame(strsplit(res[,1],"-")))
					obj1<-join.columns(tmp)
					obj2<-join.columns(tmp[,2:1])
					x<-unlist(sapply(1:length(obj1), function(i){
						tmp<-c(1:length(obj1))[sapply(1:length(obj2),function(j){
							obj2[j]%in%obj1[i]})]
						tmp[tmp>=i]
						}))	
					res[x,]
				}
	
	tmp<-out.put[,-1,drop=FALSE]
	out<-do.call("rbind",lapply(1:ncol(tmp),function(i)
		{
			obj<-results(x=tmp[,i,drop=FALSE],factor1=out.put[,1],variable=data[,i],factor2=factor, calc=calc,val=val)
			data.frame(variable=colnames(tmp)[i],t(data.frame(obj)))
		}))
	rownames(out)<-NULL	
	# adj<-p.adjust(p=fixln(out[,4]), method = "BH", n = length(fixln(out[,4])))	
	# out$adjusted.p.value<-signif(adj,4)
	out
}

#main power analysis fxn
# currently limited to single factor (t-tests and one-way ANOVA) analysis
poweranalysis <- reactive({

	#calculate values from the data
	if(input$power_calculation == "data"){
		if(is.null(input$power_variables) | is.null(input$power_factor)) return(cat("Please select data to use for the analysis."))
		data<-getdata()[,input$power_variables,drop=FALSE]
		test.factor<-getdata()[,input$power_factor,drop=FALSE]
		if(input$input_calculation_type == "alpha") {val<-input$input_power} else {val<-input$input_alpha}
		out<-tryCatch(power.analysis.from.data(data,test.factor, calc=input$input_calculation_type, val=val),error=function(e) {"Can not calculate."})
	}
	
	#calculate values based on the inputs
	if(input$power_calculation == "inputs"){
		#inputs
		effect<-input$input_effect
		alpha<-input$input_alpha
		n<-input$input_n
		power<-input$input_power
		k<-input$input_k
		cor<-input$input_correlation
		#switch to calculate
		switch(input$input_calculation_type,
			power 	= power<-NULL,
			alpha 	= alpha<-NULL,
			n		= n<-NULL,
			effect	= effect<-NULL,
			k	= k<-NULL,
			cor = cor <-NULL)
			
		if(input$power_test_type == "t.test"){
			#t-test
			type <- input$power_t_test_type
			result<-tryCatch(pwr.t.test(n = n, d = effect, sig.level = alpha, power = power, type = type),error = function(e) {NULL})
			out<-tryCatch(data.frame(samples.per.group=round(result$n,0),effect.size = result$d, alpha = result$sig.level, power = result$power),
				error = function(e) {"Could not be calculated."})
		}
		
		if(input$power_test_type == "anova"){
			#ANOVA
			result<-tryCatch(pwr.anova.test(k = k, n = n, f = effect, sig.level = alpha, power = power),error = function(e) {NULL})
			out<-tryCatch(data.frame(number.of.groups = result$k, samples.per.group=round(result$n,0),"effect.size" = result$f, alpha = result$sig.level, power = result$power),
					error = function(e) {"Could not be calculated."})
		}
		
		if(input$power_test_type == "correlation"){
			#correlation
			result<-tryCatch(pwr.r.test(n = n, r = cor, sig.level = alpha, power = power),error = function(e) {NULL})
			out<-tryCatch(data.frame(sample.size = round(result$n,0), correlation=result$r, alpha = result$sig.level, power = result$power),
					error = function(e) {"Could not be calculated."})
		}
	}
	
	#save object
	if(!is.null(out)){ # need to fix power analysis to handle single row data frame?
		if(!out=="Can not calculate.")	{
			if(input$power_calculation == "inputs"){
				name<-"poweranalysis"
				values[[name]]<-data.frame(out)
				values[["data_poweranalysis_object"]]<-unique(c(values[["data_poweranalysis_object"]],name))
			} else{
				name<-paste0(input$datasets,"_poweranalysis")
				values[[name]]<-afixlnf(out)
				values[["data_poweranalysis_object"]]<-unique(c(values[["data_poweranalysis_object"]],name))
			}
		}
	}
	return(out)
	
})

###UI objects
output$power_factor<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()
		selectInput("power_factor", "Factor",var.opts)
	 })	
	 
#variables to test
output$power_variables<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()
		#limit to non-factors?
		selectInput("power_variables", "Variables",var.opts, multiple = TRUE)
	 })	

#population size
output$input_n<-renderUI({
	numericInput(inputId="input_n", label="population size", value=32, min = 1, max = NA,step = 1)
	 })

#power
output$input_power<-renderUI({
	numericInput(inputId="input_power", label="power", value=0.8, min = 0, max = 1,step = 0.01)
	 })	

#alpha
output$input_alpha<-renderUI({
	numericInput(inputId="input_alpha", label="p-value", value=0.05, min = 0, max = 1,step = 0.001)
	 })	

#number of groups
output$input_k<-renderUI({
	numericInput(inputId="input_k", label="Number of groups", value=2, min = 0, max = 1000,step = 1)
	 })	

#effect size
output$input_effect<-renderUI({
	numericInput(inputId="input_effect", label="effect size", value=2, min = 0, max = 100,step = 0.05)
	 })	 
	 
#correlation
output$input_correlation<-renderUI({
	numericInput(inputId="input_correlation", label="correlation", value=.6, min = 0, max = 1,step = 0.005)
	 })	 
	 
#main visualize UI				
ui_poweranalysis <- function() {
	wellPanel(
		radioButtons("power_calculation", "", choices=list("From data" = "data","From inputs" = "inputs"), selected = "From data"),
		conditionalPanel("input.power_calculation == 'inputs'",
			selectInput("power_test_type","Test type:",choices=list("t-Test" = "t.test","ANOVA" = "anova", "correlation" = "correlation"), selected = "t-Test")
		),
		conditionalPanel(
				condition = "input.power_test_type == 't.test' && input.power_calculation == 'inputs'",	
				 selectInput(
						"power_t_test_type", "Type:",
						c("two sample" = "two.sample", "one sample" = "one.sample", "paired" = "paired"),		
						selected  		= "two sample"
							)
		),
		
		 selectInput(
			"input_calculation_type", "Calculate:",
			choices = list("Loading"),		
			selected  		= "Loading"
		 ),
		conditionalPanel(
				condition = "input.power_calculation == 'data'",
				uiOutput("power_factor"),
				uiOutput("power_variables")
		),	
		# input UIs
		conditionalPanel(
				condition = "input.input_calculation_type != 'power'",
				uiOutput("input_power")
		),
		conditionalPanel(
				condition = "input.input_calculation_type != 'k' && input.power_test_type == 'anova'",
				uiOutput("input_k")
		),
		conditionalPanel(
				condition = "input.input_calculation_type != 'n' & input.power_calculation == 'inputs'",
				uiOutput("input_n")
		),
		conditionalPanel(
				condition = "input.input_calculation_type != 'alpha'",
				uiOutput("input_alpha")
		),
		conditionalPanel(
				condition = "input.input_calculation_type != 'effect'& input.power_calculation == 'inputs'",
				uiOutput("input_effect")
		),
		conditionalPanel(
			condition = "input.power_test_type == 'correlation' && input.power_calculation == 'inputs'",
				uiOutput("input_correlation")
			),
		conditionalPanel(
			condition = "input.input.power_calculation == 'data'",
				actionButton("save_poweranalysis_results", "Save changes")
			)	
	)
}	 


# #update UI based on selection
observe({
	if(is.null(input$power_test_type)) return(list("Loading"))
	if(input$power_test_type == 'correlation'){
		updateSelectInput(session = session,"input_calculation_type",choices = list("power" 	= "power",
				# "number of groups"   = "k",
				"sample size" 	= "n",
				"p-value" 		= "alpha",
				"correlation" = "cor"), selected="power")		
	} else {
		updateSelectInput(session = session,"input_calculation_type",choices = list("power" 	= "power",
				"samples size" 	= "n",
				"p-value" 		= "alpha",
				"effect size" = "effect"), selected ="power")
	}
	
	if(input$power_calculation == "data"){
		updateSelectInput(session = session,"input_calculation_type",choices = list("power" = "power"), selected ="power")
	}
	
	#save results
	if(!is.null(input$save_poweranalysis_results)||!input$save_poweranalysis_results==0||!is.null(values[["data_poweranalysis_object"]])){
		isolate({
			values[["datasetlist"]]<-unique(c(values[["datasetlist"]],values[["data_poweranalysis_object"]]))
		})
	}
	
})

# dataview visualizer
output$ui_poweranalysis <- renderUI({
	ui_poweranalysis()
})

plot_control.poweranalysis <- function(){
	NULL
}

plot.poweranalysis<-function(results){
	plot(x = 1, type = 'n', main="This analysis doesn't currently support any visualization.", axes = FALSE, xlab = "", ylab = "")
}