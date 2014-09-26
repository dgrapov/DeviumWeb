
#objects for debugging the horrible mess of a graphing function below
tests<-function(){
data(mtcars)
data<-mtcars
data$am<-factor(data$am)
data$vs<-factor(data$vs)
plot.obj<-input<-list()
plot.obj$data<-data
plot.obj$xvar<-data[,1]
plot.obj$yvar<-data[,2]
plot.obj$group<-factor(data$am)
plot.obj$plot.type<-"scatter_plot"
plot.obj$size.mapping<-"absolute"
font.size<-1


input$y_facet<-"vs"
input$x_facet<-"am"
input$group_var<-"am"
input$x_var<-colnames(data)[1]
input$y_var<-colnames(data)[2]
size.lab<-""

}

#main convoluted plotting fxn
#should break up into many plot type specific fxns (e.g. box plot, scatter plot, etc)
make.ggplot<-function(input) { 
	
	# many vars could referenced directly from input
	# started a bad example of going through plot.object, too lazy to fix for now
	plot.obj<-list()
	# need:
	plot.obj$plot.type<-input$plot_type
	#data
	plot.obj$data<-getdata()
	plot.obj$xvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$x_var]
	plot.obj$yvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$y_var]
	
	
	# variables to map
	#-----------------
	#some conditional variables may not have been loaded yet and will error if?
	plot.obj$group<-plot.obj$data[,colnames(plot.obj$data)%in%input$group_var] # color
	if(plot.obj$plot.type == "scatter_plot"){
		plot.obj$size<-if(input$size_mapping=="variable"){
				size.lab<-input$size_variable
				plot.obj$data[,colnames(plot.obj$data)%in%input$size_variable]	
			} else {
				size.lab<-""
				input$size			
			}
	} 
		
	#bar plot
	plot.obj$sort_bar_plot<-input$sort_bar_plot	
	plot.obj$flip_plot_coordinates<-input$flip_plot_coordinates
	
	#variable for grouping visualizations	
	plot.obj$group_type<-input$stat_type
	if(is.null(plot.obj$group_type)){plot.obj$group_type<-"none"}
	
	#labels for scatterplots
	if(!is.null(input$viz_labels_text)){
		if(input$viz_labels_text=="none"){plot.obj$labels<-NULL}
		if(input$viz_labels_text=="rownames"){plot.obj$labels<-rownames(getdata())}
		if(!input$viz_labels_text%in%c("none","rownames")){plot.obj$labels<-getdata()[,input$viz_labels_text]}
	} else { 
		plot.obj$labels<-NULL 
	}
	
	# asthetics
	plot.obj$show.points<-input$show_points
	plot.obj$alpha<-input$alpha # points
	plot.obj$size.mapping<-input$size_mapping
	

	
	#create ggplot data object
	#need to make sure input is not NULL and has length != 0
	#main data object (has to be a better way)
	tmp.data<-data.frame( 	group	= if(length(plot.obj$group)>0){ join.columns(plot.obj$group) } else { data.frame(group=0) }, # color bad naming
							yvar	= if(length(plot.obj$yvar)>0){ 	plot.obj$yvar } else { data.frame(yvar=0) }, 
							xvar	= if(length(plot.obj$xvar)>0){ 	plot.obj$xvar } else { data.frame(xvar=0) },
							size	= if(length(plot.obj$size)>0){ plot.obj$size } else { data.frame(size=0) },
							labels	= if(length(plot.obj$labels)>0){ plot.obj$labels } else { data.frame(labels="") },
							facet.y	= if(length(input$y_facet)>0){ 
										if(!input$y_facet ==".") {	
												plot.obj$data[,colnames(plot.obj$data)%in%input$y_facet,drop=FALSE]
											} else { 
												data.frame(y.facet=0) 
											}
										},
							facet.x	= if(length(input$x_facet)>0){ 
										if(!input$x_facet ==".") {	
												plot.obj$data[,colnames(plot.obj$data)%in%input$x_facet,drop=FALSE]
											} else { 
												data.frame(x.facet=0) 
											}
										}			
							)
	
	#set order for drawing groups
	tmp.data$group<-factor(tmp.data$group,levels=unique(tmp.data$group), ordered=TRUE)
	
	#label scatter plot points
	lab.offset<-input$viz_labels_text_y_offset
	font.size<-input$viz_labels_text_size
	tmp.data$lab.y<-tmp.data$yvar-lab.offset	
							
	
	#switch ggplot add ons specific to each plot type 
	type<-as.character(plot.obj$plot.type)
	plot.type<-switch(type,
			"box_plot" 		= 	.local<-function(tmp){geom_boxplot()},
			"histogram" 	=	.local<-function(tmp){geom_histogram(alpha=plot.obj$alpha,position="identity",binwidth=input$plot_binwidth)},
			"density_plot" 	=	.local<-function(tmp){geom_density(alpha=plot.obj$alpha)},
			"bar_plot" 		=	.local<-function(tmp){geom_bar(alpha=plot.obj$alpha,position="dodge",stat = "identity")}, # width is not working
			"violin_plot"	= 	.local<-function(tmp){geom_violin()},
			"dot_plot"		= 	.local<-function(tmp){geom_dotplot(alpha=plot.obj$alpha,binwidth=input$plot_binwidth)},
			"scatter_plot"  =	.local<-function(tmp){
									if(tmp$size.mapping=="absolute"){
										geom_point(alpha=plot.obj$alpha, size=plot.obj$size)
									} else {
										geom_point(alpha=plot.obj$alpha, aes(size=size))
									}
								}	
		)
	plot.type<-plot.type(plot.obj)
	
	#facet type 
	facet.type<-paste(input$y_facet, '~', input$x_facet)
	if (facet.type != '. ~ .') {facet.type<-facet_grid(facet.type)} else {facet.type<-NULL}

	
	#color pallet 
	#right now every thing is treated as a discreet variable
	#color and fill both map to the same single variable input$group_var
	#create color scale based on the chosen pallet
	if(is.null(input$plot_color_pallet)|is.null(input$group_var)) {
		color.scale<-NULL
	} else {	
		ncol<-if(is.null(getdata()[,input$group_var,drop=FALSE]))  1 else length(unique(join.columns(getdata()[,input$group_var,drop=FALSE])))
		if(input$plot_color_pallet=="rainbow"){
			.colors<-rainbow(ncol)
		} else {
			.colors<-brewer.pal(ncol,input$plot_color_pallet)
			
		}
		if(type=="scatter_plot"){
			# if(is.null(input$stat_type)){
				color.scale<-scale_color_manual(values=.colors)
			# } else {
				# color.scale<-scale_color_manual(values=.colors)+ scale_fill_manual(values=.colors,guide="none")
			# }	
		} else {
			color.scale<-scale_fill_manual(values=.colors) 
		}	
	}	
	
	#plotting theme
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
				 
	if(any(type%in%c("violin_plot","box_plot")))	{		#control for 1D or 2D graphs 
		p<-ggplot(tmp.data, 
				aes(
					x 		= group, 
					y 		= xvar,
					fill 	= as.factor(group)
					)
				) + plot.type 
				
				if(plot.obj$show.points==TRUE)
				{ 
					p<-p+ geom_point(color='black',alpha=plot.obj$alpha, position = 'jitter')
				}
		} 
		
		if(any(type%in%c("density_plot","histogram","dot_plot"))){	
			p<-ggplot(tmp.data, 
					aes(
						x 		= xvar,
						fill 	= as.factor(group)#,
						# group 	= as.factor(group),
						# color 	= as.factor(plot.obj$group)
						)
					) + plot.type 
					
		}
		
		if(type=="bar_plot"){
			tmp.obj<-data.frame(.index=c(1:nrow(tmp.data)),labels=rownames(plot.obj$data),tmp.data)
			
			#sort on magnitude
			if(plot.obj$sort_bar_plot==TRUE){	
					# tmp.obj<-tmp.obj[order(tmp.data$xvar,decreasing=TRUE),]
					# tmp.obj$.index<-c(1:nrow(tmp.data))
					 tmp.obj$labels <- factor(fixlc(tmp.obj$labels),levels=fixlc(tmp.obj$labels)[order(tmp.obj$xvar,decreasing=TRUE)])
				} else {bar_sort<-NULL}
			
			p<-ggplot(tmp.obj, 
					aes(
						x 		= labels,
						y 		= xvar,
						fill 	= as.factor(group)#,
						# group 	= as.factor(group),
						# color 	= as.factor(plot.obj$group)
						)
					) + plot.type
			}
		
		#flip coordinates for plots above
		if(!type=="scatter_plot"&plot.obj$flip_plot_coordinates==TRUE){
			p<-p+coord_flip()	
		} else { 
			p<-p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) # may not need
		}
		
		#scatter plot 
		if(type=="scatter_plot"){
			p<-ggplot(tmp.data, 
				aes(
					x 		= xvar,
					y 		= yvar,
					size 	= size,	
					# fill 	= group#,
					# group 	= as.factor(group),
					color 	= factor(group)
					)
				) 			 
		
			#grouping geom for scatter plots
			if("ellipse"%in%plot.obj$group_type) {
				p<-p + stat_ellipse(aes(fill=factor(group)),geom="polygon",alpha=0.3,size=.5,show_guide=FALSE)
			}
		
			if("bin_hex"%in%plot.obj$group_type) {
				p<-p+stat_binhex(aes(fill=factor(group)),show_guide=FALSE,alpha=0.3)
			}	
			
			if("dens_2d"%in%plot.obj$group_type) {
				p<-p + geom_density2d(aes(color=factor(group)),alpha=.5,size=.5,show_guide=FALSE)
			}
		
			#handling lm and loess smoothers

			if("lm_mod"%in%plot.obj$group_type){
				p<-p+stat_smooth(method=lm, aes(fill = as.factor(group)),size=1,show_guide=FALSE)
			}	
			
			if("loess_mod"%in%plot.obj$group_type){
				p<-p+stat_smooth(aes(fill = as.factor(group)),size=1,show_guide=FALSE)
			
			}
				
			#add points and text last
			p<-p + plot.type
				
			#add labels to points
			# if(!all(tmp.data$labels=="")){
			p<-p+geom_text(size=font.size,aes(x=xvar, y=lab.y,label=labels),color="black",show_guide = FALSE)
		}
		
		#labels
		if(any(type%in%c("violin_plot","box_plot","bar_plot"))){
			labels<-labs(
					fill 	= input$group_var,
					x 		= "",
					y 		= input$x_var
				)  
		} 
		if(type=="scatter_plot"){
			labels<-labs(
					color 	= input$group_var,
					x 		= input$x_var,
					y 		= input$y_var,
					size 	= size.lab
				) 
		}		
				
		if(any(type%in%c("density_plot","histogram","dot_plot"))){
			labels<-labs(
					fill 	= input$group_var,
					x 		= input$x_var#,
					# y 		= input$y_var
				)  
		}
		
	#initialize plot	
	 p<- p+ labels +.theme
	

	# default color/fill based on group should be handled based on variable class
	if (all(tmp.data$group==0)){
		if(type=="scatter_plot"){
				p<-p+scale_color_manual(values="#6495ED",guide = "none") + scale_fill_manual(values="#6495ED",guide = "none")
			} else{
				p<-p+scale_fill_manual(values="#6495ED",guide = "none")
			}
	}
	
	
	#size
	# if(!all(na.omit(fixln(tmp.data$size))==0)){
	if(!is.null(tmp.data$size)&!all(tmp.data$size==0)){
		if(is.factor(tmp.data$size)){
			p<-p+scale_size_discrete(range = c(input$variable_size_min,input$variable_size_max)) # c(input$variable_size_min,input$variable_size_max)
		} else{
			p<-p+scale_size_continuous(range =c(input$variable_size_min,input$variable_size_max)) #  c(input$variable_size,input$variable_size)
		}	
	}
	
	#add facets and color scale
	p<-p + facet.type + color.scale
	print(p)
}

#functions controlling various output
plot.visualize <- function(result){
	if(is.null(input$datasets)) return()
	if(input$analysistabs != 'Plots') return()
	
	make.ggplot(values$visualize.objects)
	
}#, width = viz_plot_width, height = viz_plot_height)

summary.visualize <- function(result) {
	# str(get(values$visualize.objects$datasets))
	str(values[[values$visualize.objects$datasets]])
}

#collect all UI inputs to send to summary and plotting
visualize<-reactive({
	
	#set values to control plot dimensions
	if(input$tool=="visualize") {
		values$plot$'plotWidth'<-viz_plot_width()
		values$plot$'plotHeight'<-viz_plot_height()
	}	
	#values storage...
	values$visualize.objects<-tryCatch(copy.input(),error=function(e){NULL})
	return(values$visualize.objects)
	# values$visualize.objects<-"crap"#copy.input()

})

#save plot
save.plot.name<-reactive({
	if(is.null(input$datasets)) return()
	# paste(input$datasets,input$plot_type,sep="-")
	sprintf("%s_%s.%s",input$datasets,input$plot_type,'pdf')
	})

output$Download_vis_plot<-downloadHandler(
      filename = save.plot.name,
    # a function that actually opens a connection to a pdf and print the result to it
    content = function(FILE=NULL) {
	
		# make.ggplot()
		# ggsave(file="plot.pdf")#FILE
		
	    pdf(file=FILE, onefile=T, width=12,height=8)
		# print(make_PC_Plot())
		make.ggplot(values$visualize.objects)
		dev.off()
	}
)

#output dimensions
viz_plot_width <- reactive({
        if(is.null(input$viz_plot_width)) return(650)
    
		 input$viz_plot_width
})

viz_plot_height <- reactive({
        if(is.null(input$viz_plot_height)) return(650)
        # if(input$viz_multiple) { 
                # nrPlots <- length(input$vizvars1)
                # ifelse(nrPlots > 1, return(325 * ceiling(nrPlots / 2)), return(650))
        # } else {
		input$viz_plot_height
        # }
})

# visualize<-reactive({
	# #seems like inputs need to be collected here?
	
	# # quote({ 
		# return(list('plotHeight' =viz_plot_height(),'plotWidth' =viz_plot_width() ))
	# # })
# })

# # dataview visualizer
# output$ui_visualize <- renderUI({
	# ui_visualize()
# })

#shared UIs
#alpha slider 
output$alpha.ui<-renderUI({
if (is.null(getdata())){return()}
	numericInput("alpha", 
				"Transparency", 
				min = .1,
				max = 1, 
				step = .05,
				value = .75)	
	})

# seems that UIs can not be shared between multiple functions 
# need to create redundant UIs or think of holistic hide show interface
# x-axis
output$x_var<-renderUI({
	if (is.null(getdata())){return()}
			var.opts<-varnames()#colnames(get(input$data))#
			selectInput("x_var", "X-variable", var.opts)
			# updateSelectInput(session, "variable", choices = var.opts)
	 })

#y-axis	 
output$y_var<-renderUI({
	if (is.null(getdata())){return()}
	#don't allow factors for now
	#error: 'x' and 'units' must have length > 0 when y is a factor? 
	var.opts<-varnames()#colnames(get(input$data))#
	is.num<-sapply(getdata(),is.numeric)
	is.int<-sapply(getdata(),is.integer)
	var.opts<-var.opts[is.num|is.int]
	selectInput("y_var", "Y-variable", var.opts)
		# updateSelectInput(session, "variable", choices = var.opts)
	 })	

#group used for color/fill
output$group_var<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()#colnames(get(input$data))#
		selectInput("group_var", "",c( none="", var.opts),multiple = TRUE)
	 })	

#get possible pallets based on factor length
pallet.options<-function(n=3){
	c("rainbow",rownames(brewer.pal.info)[brewer.pal.info$maxcolors>=n])
}	 
	 
#a dynamic color pallet chooser based on the number of levels of a factor
output$plot_color_pallet<-renderUI({
	if(is.null(input$group_var)) return()
	opts<-pallet.options(1)
	selectInput("plot_color_pallet", "pallet",choices = opts, selected="rainbow", multiple = FALSE)
})	

#observe input for group_var and update options for pallet
observe({
	if(is.null(input$group_var)) return()
	isolate({
		if(is.null(input$plot_color_pallet)) return()
		n<-length(unique(join.columns(getdata()[,input$group_var,drop=FALSE])))
		opts<-pallet.options(n)
		updateSelectInput(session, "plot_color_pallet", "pallet", choices = opts ,selected="rainbow")
	})
}) 
	 
#map variable to size
output$size_variable<-renderUI({
				if (is.null(getdata())){return()}
				var.opts<-varnames()#colnames(get(input$data))#
				selectInput("size_variable", "", var.opts)
})

#size
output$size_ui<-renderUI({ 
	if (is.null(getdata())){return()}
	# if(is.null(input$size_mapping)) return()
	wellPanel(
		radioButtons("size_mapping","", choices = c(absolute="absolute",variable="variable"), selected = "absolute"),
		conditionalPanel(
			condition = "input.size_mapping == 'absolute'",
			numericInput("size", 
								"", 
								min = 1,
								max = 30,
								step = 1,					
								value = 5)	
		),
		conditionalPanel(
				condition = "input.size_mapping == 'variable'",
				uiOutput("size_variable"),
				div(class="row-fluid", # see css.style which actually makes this work?
                    div(class="span6",numericInput("variable_size_min", label = "min", min =0,max=40, step = 1, value = 2)),
					div(class="span6", numericInput("variable_size_max", label = "max", min = 0,max=40, step = 1, value = 8))
				)
				# sliderInput("variable_size", # not rendering correctly
								# "", 
								# min = 1,
								# max = 30,
								# step = 1,					
								# value = c(3,10))
			) 
	)
})

# y-facet
output$y_facet<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()#colnames(get(input$data))#
		factor.id <- sapply(getdata(), is.factor)
		var.opts<-var.opts[factor.id]
		selectInput("y_facet", "Y-facet", c(none=".",var.opts))
		# updateSelectInput(session, "variable", choices = var.opts)
	 })	

# x-facet
output$x_facet<-renderUI({
		var.opts<-varnames()#colnames(get(input$data))#
		factor.id <- sapply(getdata(), is.factor)
		var.opts<-var.opts[factor.id]
		selectInput("x_facet", "X-facet", c(none=".",var.opts))
	 })	

#labels	 
output$viz_labels<-renderUI({
		vars<-varnames()#colnames(get(input$data))#
		selectInput("viz_labels_text", "Text", c(none="none",rownames="rownames",vars),selected="none")
	 })	
	 
#main visualize UI				
ui_visualize <- function() {

list(
	# downloadButton('Download_vis_plot', label = "Download plot"),
	# br(),
	# h4(''),
	wellPanel(
	tags$details(open="open",tags$summary("Plot"),	
			 selectInput(
			"plot_type", "Type:",
			c(
			"Bar plot" 		= "bar_plot",
			"Histogram" 	= "histogram",
			"Dot plot" 		= "dot_plot",
			"Density plot" 	= "density_plot",
			"Box plot" 		= "box_plot",
			"Violin plot"	= "violin_plot",
			"Scatter plot" 	= "scatter_plot"),		
			selected  		= "Bar plot"
			 ),
			# interweaving UIs
			uiOutput("x_var"),
			conditionalPanel(
					condition = "input.plot_type == 'scatter_plot'", # TODO: add dot plot interface
					uiOutput("y_var")
				),
			#show points
			conditionalPanel(
					condition = "input.plot_type == 'box_plot'|input.plot_type == 'violin_plot'",		
					checkboxInput("show_points", "show points", TRUE)
			),
			#sort bar plot
			conditionalPanel(
					condition = "input.plot_type == 'bar_plot'",		
					checkboxInput("sort_bar_plot", "sort", FALSE)
			),
			#flip coordinates
			conditionalPanel(
					condition = "input.plot_type != 'scatter_plot'",		
					checkboxInput("flip_plot_coordinates", "flip coordinates", FALSE)
			),
			#binwidth
			conditionalPanel(
					condition = "input.plot_type == 'histogram'|input.plot_type == 'dot_plot'",		
					numericInput("plot_binwidth", "binwidth",min=.1,value=1.5,step=.25)
			)	
		)
	),
	wellPanel(	
		tags$details(tags$summary("Color"),
					uiOutput("group_var"),
					uiOutput("plot_color_pallet"),
					uiOutput("alpha.ui")
			)
	),
	conditionalPanel(condition = "input.plot_type == 'scatter_plot'",
		wellPanel(	
		tags$details(tags$summary("Size"),
					uiOutput("size_ui")
				)
		)
	),
	conditionalPanel(condition = "input.plot_type == 'scatter_plot'",
		wellPanel(
			tags$details(tags$summary("Stats"),	
			 selectInput(
				"stat_type", "",
				c("none" 		= "",
				"ellipse" 	= "ellipse",
				# "polygon" 	= "polygon",
				"linear model" 		= "lm_mod",
				"loess smoother" 	= "loess_mod",
				"bin hexagon"		= "bin_hex",
				"2D density"		= "dens_2d" ),		
				selected  		= "none", multiple=TRUE
			 )
			)
		),
		wellPanel(
			tags$details(tags$summary("Labels"),	
				uiOutput("viz_labels"),
				numericInput("viz_labels_text_y_offset", "vertical offset", step = 1,value = 1),
				numericInput("viz_labels_text_size", "size", min = 0,step = 1,	value = 5)
			)
		)
	),
	wellPanel(
		tags$details(tags$summary("Facets"),	
			uiOutput("y_facet"),
			uiOutput("x_facet")
		)
	),
	wellPanel(
		tags$details(tags$summary("More options"),	
		div(class="row-fluid", # see css.style for this as well
			  div(class="span6",numericInput("viz_plot_height", label = "Plot height:", min = 100, step = 50, value = 650)),
			  div(class="span6", numericInput("viz_plot_width", label = "Plot width:", min = 100, step = 50, value = 650))
			),
			br(),
			downloadButton('Download_vis_plot', label = "Download plot")
		)
	)	
)
}
	
# # dataview visualizer
# output$ui_visualize <- renderUI({
	# ui_visualize()
# })
