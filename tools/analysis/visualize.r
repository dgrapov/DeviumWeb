
#main plotting function
# #could pass ars directly (fast and easy, Shiny only) or collect in an another object and then pass (more universal)
DEPRECATEDmake.ggplot<-function() { 
	
	# many vars could referenced directly from input
	plot.obj<-list()
	# need:
	plot.obj$plot.type<-input$plot_type
	#data
	plot.obj$data<-getdata()
	plot.obj$xvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$x_var]
	plot.obj$yvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$y_var]
	# variables to map
	plot.obj$group<-plot.obj$data[,colnames(plot.obj$data)%in%input$group_var] # color
	plot.obj$size<-if(input$size_mapping=="variable"){
			size.lab<-input$size_variable
			plot.obj$data[,colnames(plot.obj$data)%in%input$size_variable]	
		} else {
			size.lab<-""
			input$size			
		}
	#bar plot
	plot.obj$sort_bar_plot<-input$sort_bar_plot	
	plot.obj$verticle_bar_plot<-input$vertical_bar_plot
	
	#variable for grouping visualizations	
	plot.obj$group_type<-input$stat_type
	
	# asthetics
	plot.obj$show.points<-input$show_points
	plot.obj$alpha<-input$alpha # points
	plot.obj$size.mapping<-input$size_mapping
	
	
	#create ggplot data object
	#need to make sure input is not NULL and has length != 0
	#main data object (has to be a better way)
	tmp.data<-data.frame( 	group	= if(length(plot.obj$group)>0){ plot.obj$group } else { data.frame(group=0) }, # color bad naming
							yvar	= if(length(plot.obj$yvar)>0){ 	plot.obj$yvar } else { data.frame(yvar=0) }, 
							xvar	= if(length(plot.obj$xvar)>0){ 	plot.obj$xvar } else { data.frame(xvar=0) },
							size	= if(length(plot.obj$size)>0){ plot.obj$size } else { data.frame(size=0) },
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
							
	
	#switch ggplot add ons specific to each plot type 
	type<-as.character(plot.obj$plot.type)
	plot.type<-switch(type,
			"box_plot" 		= 	.local<-function(tmp){geom_boxplot()},
			"histogram" 	=	.local<-function(tmp){geom_histogram(alpha=plot.obj$alpha,position="identity")},
			"density_plot" 	=	.local<-function(tmp){geom_density(alpha=plot.obj$alpha)},
			"bar_plot" 		=	.local<-function(tmp){geom_bar(alpha=plot.obj$alpha,position="dodge",stat = "identity")}, # width is not working
			"scatter_plot"  =	.local<-function(tmp){
									if(tmp$size.mapping=="absolute"){
										geom_point(alpha=plot.obj$alpha, size=plot.obj$size)
									} else {
										geom_point(alpha=plot.obj$alpha, aes(size=size))
									}
								}	
		)
	plot.type<-plot.type(plot.obj)
	
	#facet type this will effect what is considered a group in the polygon visualizations
	facet.type<-paste(input$y_facet, '~', input$x_facet)
	if (facet.type != '. ~ .') {facet.type<-facet_grid(facet.type)} else {facet.type<-NULL}

	
	#plotting theme
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
				 
	if(type=="box_plot")	{		#control for 1D or 2D graphs 
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
				
				
		} else {
			
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
			tmp.obj<-data.frame(.index=c(1:nrow(tmp.data)),tmp.data)
			if(plot.obj$sort_bar_plot==TRUE){	
					tmp.obj<-tmp.obj[order(tmp.data$xvar,decreasing=TRUE),]
					tmp.obj$.index<-c(1:nrow(tmp.data))
				}
			p<-ggplot(tmp.obj, 
					aes(
						x 		= .index,
						y 		= xvar,
						fill 	= as.factor(group)#,
						# group 	= as.factor(group),
						# color 	= as.factor(plot.obj$group)
						)
					) + plot.type 
			if(plot.obj$verticle_bar_plot==TRUE){ p<-p+coord_flip()	}
		}
	
		if(type=="scatter_plot"){
			p<-ggplot(tmp.data, 
				aes(
					x 		= xvar,
					y 		= yvar,
					size 	= size,	
					# fill 	= group#,
					# group 	= as.factor(group),
					color 	= as.factor(group)
					)
				) + plot.type
		} 
		#labels
		if(type=="box_plot"){
			labels<-labs(
					fill 	= input$group_var,
					x 		= "",
					y 		= input$x_var
				)  
		} 
		if(type=="bar_plot"){
			labels<-labs(
					fill 	= input$group_var,
					x 		= "index",
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
				
		if(type=="density_plot"|type=="histogram"){
			labels<-labs(
					fill 	= input$group_var,
					x 		= input$x_var#,
					# y 		= input$y_var
				)  
		}
		
	#initialize plot	
	 p<- p+ labels +.theme
	
	pos.x<-pos.y<-NULL
	
	#handling lm and loess smoothers
	if(type=="scatter_plot"){
		if(plot.obj$group_type=="lm_mod"){
			p<-p+stat_smooth(method=lm, aes(fill = as.factor(group)),size=1,show_guide=FALSE)
		}
		if(plot.obj$group_type=="loess_mod"){
			p<-p+stat_smooth(aes(fill = as.factor(group)),size=1,show_guide=FALSE)
		
		}
		
	}	
		
	
	#add polygon or ellipse visualizations while accounting for faceting
	if(plot.obj$group_type=="ellipse"|plot.obj$group_type=="polygon"){
		if(type=="scatter_plot"){
			if(is.null(facet.type)){ #no facets
				if(is.factor(tmp.data$group) | length(unique(tmp.data$group))<=(length(tmp.data$group)/3)){ # trying to control errors from too few points in ellipse
					if(plot.obj$group_type=="ellipse"){# group visualization via Hoettellings T2 ellipse
							ell<-tryCatch(get.ellipse.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp.data$group)$coords, error=function(e){NULL})
						}
					if(plot.obj$group_type=="polygon"){
							ell<-tryCatch(get.polygon.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp.data$group), error=function(e){NULL})	
					}
					if(!is.null(ell)){
						p<-p+geom_polygon(data=data.frame(ell),aes(x=x,y=y,fill=group,color=group),alpha=.1, size=.1, legend=FALSE) 
					}
				}
			} else { #with faceting
				pos<-1
				tmp<-tmp.data$group
				if(!all(tmp.data$group==0)) {tmp<-plot.obj$group} 
				
				if(is.null(tmp.data$y.facet)){ 
						tmp<-cbind(tmp,plot.obj$data[,colnames(plot.obj$data)%in%input$y_facet,drop=FALSE])
						pos.y<-pos<-pos+1
						
					} else {pos.y<-NULL}
				if(is.null(tmp.data$x.facet)){ 
						tmp<-cbind(tmp,plot.obj$data[,colnames(plot.obj$data)%in%input$x_facet,drop=FALSE])
						pos.x<-pos<-pos+1
					} else {pos.x<-NULL}
					
				tmp<-join.columns(tmp)
				if(plot.obj$group_type=="ellipse"){
					ell<-tryCatch(get.ellipse.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp)$coords, error=function(e){NULL})# group visualization via Hoettellings T2 ellipse
				}
				if(plot.obj$group_type=="polygon"){
					ell<-tryCatch(get.polygon.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp), error=function(e){NULL})
				}
				# values$A1<-ell
					if(!is.null(ell)){
						#try adding facet var
						facet.match<-data.frame(do.call("rbind",strsplit(fixlc(ell$group),"\\|")))
						# values$A2<-facet.match
						# facet.match<-facet.match[,c(1:ncol(facet.match))%%2==1] 
						# values$A3<-facet.match
						if(!is.null(pos.y)) ell[[input$y_facet]]<-facet.match[,pos.y,drop=TRUE]
						# values$A4<-ell
						if(!is.null(pos.x)) ell[[input$x_facet]]<-facet.match[,pos.x,drop=TRUE]
						p<-p+geom_polygon(data=data.frame(ell),aes(x=x,y=y,fill=group),alpha=.2, size=.1, color="gray60", legend=FALSE) 
					}
			}	
		}
	}
	
	#control scales for plotting discreete vs. continuous mappings
	# color/fill based on group should be handled based on variable class
	if (all(tmp.data$group==0)){
		if(type=="scatter_plot"){
				p<-p+scale_color_manual(values="#6495ED",guide = "none")
				if(is.null(pos.x) & is.null(pos.y)){
					p<-p+scale_fill_manual(values="#6495ED",guide = "none")
				}
			} else{
				p<-p+scale_fill_manual(values="#6495ED",guide = "none")
			}
	}

	#size
	if(!all(fixln(tmp.data$size)==0)){
		if(is.factor(tmp.data$size)){
			p<-p+scale_size_discrete(range = c(input$variable_size_min,input$variable_size_max)) # c(input$variable_size_min,input$variable_size_max)
		} else{
			p<-p+scale_size_continuous(range =c(input$variable_size_min,input$variable_size_max)) #  c(input$variable_size,input$variable_size)
		}	
	}
	
	#add facets
	p<-p + facet.type
	print(p)
}

make.ggplot<-function() { 
	
	# many vars could referenced directly from input
	plot.obj<-list()
	# need:
	plot.obj$plot.type<-input$plot_type
	#data
	plot.obj$data<-getdata()
	plot.obj$xvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$x_var]
	plot.obj$yvar<-plot.obj$data[,colnames(plot.obj$data)%in%input$y_var]
	# variables to map
	plot.obj$group<-plot.obj$data[,colnames(plot.obj$data)%in%input$group_var] # color
	plot.obj$size<-if(input$size_mapping=="variable"){
			size.lab<-input$size_variable
			plot.obj$data[,colnames(plot.obj$data)%in%input$size_variable]	
		} else {
			size.lab<-""
			input$size			
		}
	#bar plot
	plot.obj$sort_bar_plot<-input$sort_bar_plot	
	plot.obj$verticle_bar_plot<-input$vertical_bar_plot
	
	#variable for grouping visualizations	
	plot.obj$group_type<-input$stat_type
	
	#labels for scatterplots
	if(!is.null(input$viz_labels_text)){
		if(input$viz_labels_text=="none"){plot.obj$labels<-NULL}
		if(input$viz_labels_text=="rownames"){plot.obj$labels<-rownames(getdata())}
		if(!input$viz_labels_text%in%c("none","rownames")){values$AAAA<-plot.obj$labels<-getdata()[,input$viz_labels_text]}
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
							
	#label scatter plot points
	lab.offset<-input$viz_labels_text_y_offset
	font.size<-input$viz_labels_text_size
	tmp.data$lab.y<-tmp.data$yvar-lab.offset	
							
	
	#switch ggplot add ons specific to each plot type 
	type<-as.character(plot.obj$plot.type)
	plot.type<-switch(type,
			"box_plot" 		= 	.local<-function(tmp){geom_boxplot()},
			"histogram" 	=	.local<-function(tmp){geom_histogram(alpha=plot.obj$alpha,position="identity")},
			"density_plot" 	=	.local<-function(tmp){geom_density(alpha=plot.obj$alpha)},
			"bar_plot" 		=	.local<-function(tmp){geom_bar(alpha=plot.obj$alpha,position="dodge",stat = "identity")}, # width is not working
			"scatter_plot"  =	.local<-function(tmp){
									if(tmp$size.mapping=="absolute"){
										geom_point(alpha=plot.obj$alpha, size=plot.obj$size)
									} else {
										geom_point(alpha=plot.obj$alpha, aes(size=size))
									}
								}	
		)
	plot.type<-plot.type(plot.obj)
	
	#facet type this will effect what is considered a group in the polygon visualizations
	facet.type<-paste(input$y_facet, '~', input$x_facet)
	if (facet.type != '. ~ .') {facet.type<-facet_grid(facet.type)} else {facet.type<-NULL}

	
	#plotting theme
	.theme<- theme(
				axis.line = element_line(colour = 'gray', size = .75), 
				panel.background = element_blank(),  
				plot.background = element_blank()
				 )	 
				 
	if(type=="box_plot")	{		#control for 1D or 2D graphs 
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
				
				
		} else {
			
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
			if(plot.obj$verticle_bar_plot==TRUE){ p<-p+coord_flip()	} else (p<-p + theme(axis.text.x = element_text(angle = 90, hjust = 1)))
		}
	
		if(type=="scatter_plot"){
			#error: 'x' and 'units' must have length > 0 when y is a factor?
			# if(any(is.na(as.numeric(tmp.data$yvar)))){
				# tmp.data$yvar<-as.numeric(as.factor(unlist(tmp.data$yvar)))
				# values$Ayvar<-str(tmp.data[,"yvar",drop=F])
			# }
			p<-ggplot(tmp.data, 
				aes(
					x 		= xvar,
					y 		= yvar,
					size 	= size,	
					# fill 	= group#,
					# group 	= as.factor(group),
					color 	= as.factor(group)
					)
				) + plot.type
				
			#add labels to points
			# if(!all(tmp.data$labels=="")){
				p<-p+geom_text(size=font.size,aes(x=xvar, y=lab.y,label=labels),color="black",show_guide = FALSE)
			# }
								
		} 
		#labels
		if(type=="box_plot"){
			labels<-labs(
					fill 	= input$group_var,
					x 		= "",
					y 		= input$x_var
				)  
		} 
		if(type=="bar_plot"){
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
				
		if(type=="density_plot"|type=="histogram"){
			labels<-labs(
					fill 	= input$group_var,
					x 		= input$x_var#,
					# y 		= input$y_var
				)  
		}
		
	#initialize plot	
	 p<- p+ labels +.theme
	
	pos.x<-pos.y<-NULL
	
	#handling lm and loess smoothers
	if(type=="scatter_plot"){
		if(plot.obj$group_type=="lm_mod"){
			p<-p+stat_smooth(method=lm, aes(fill = as.factor(group)),size=1,show_guide=FALSE)
		}
		if(plot.obj$group_type=="loess_mod"){
			p<-p+stat_smooth(aes(fill = as.factor(group)),size=1,show_guide=FALSE)
		
		}
		
	}	
		
	
	#add polygon or ellipse visualizations while accounting for faceting
	if(plot.obj$group_type=="ellipse"|plot.obj$group_type=="polygon"){
		if(type=="scatter_plot"){
			if(is.null(facet.type)){ #no facets
				if(is.factor(tmp.data$group) | length(unique(tmp.data$group))<=(length(tmp.data$group)/3)){ # trying to control errors from too few points in ellipse
					if(plot.obj$group_type=="ellipse"){# group visualization via Hoettellings T2 ellipse
							ell<-tryCatch(get.ellipse.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp.data$group)$coords, error=function(e){NULL})
						}
					if(plot.obj$group_type=="polygon"){
							ell<-tryCatch(get.polygon.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp.data$group), error=function(e){NULL})	
					}
					if(!is.null(ell)){
						p<-p+geom_polygon(data=data.frame(ell),aes(x=x,y=y,fill=group,color=group),alpha=.1, size=.1, legend=FALSE) 
					}
				}
			} else { #with faceting
				pos<-1
				tmp<-tmp.data$group
				if(!all(tmp.data$group==0)) {tmp<-plot.obj$group} 
				
				if(is.null(tmp.data$y.facet)){ 
						tmp<-cbind(tmp,plot.obj$data[,colnames(plot.obj$data)%in%input$y_facet,drop=FALSE])
						pos.y<-pos<-pos+1
						
					} else {pos.y<-NULL}
				if(is.null(tmp.data$x.facet)){ 
						tmp<-cbind(tmp,plot.obj$data[,colnames(plot.obj$data)%in%input$x_facet,drop=FALSE])
						pos.x<-pos<-pos+1
					} else {pos.x<-NULL}
					
				tmp<-join.columns(tmp)
				if(plot.obj$group_type=="ellipse"){
					ell<-tryCatch(get.ellipse.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp)$coords, error=function(e){NULL})# group visualization via Hoettellings T2 ellipse
				}
				if(plot.obj$group_type=="polygon"){
					ell<-tryCatch(get.polygon.coords(cbind(tmp.data$xvar,tmp.data$yvar),tmp), error=function(e){NULL})
				}
				# values$A1<-ell
					if(!is.null(ell)){
						#try adding facet var
						facet.match<-data.frame(do.call("rbind",strsplit(fixlc(ell$group),"\\|")))
						# values$A2<-facet.match
						# facet.match<-facet.match[,c(1:ncol(facet.match))%%2==1] 
						# values$A3<-facet.match
						if(!is.null(pos.y)) ell[[input$y_facet]]<-facet.match[,pos.y,drop=TRUE]
						# values$A4<-ell
						if(!is.null(pos.x)) ell[[input$x_facet]]<-facet.match[,pos.x,drop=TRUE]
						p<-p+geom_polygon(data=data.frame(ell),aes(x=x,y=y,fill=group),alpha=.2, size=.1, color="gray60", legend=FALSE) 
					}
			}	
		}
	}
	
	#control scales for plotting discreete vs. continuous mappings
	# color/fill based on group should be handled based on variable class
	if (all(tmp.data$group==0)){
		if(type=="scatter_plot"){
				p<-p+scale_color_manual(values="#6495ED",guide = "none")
				if(is.null(pos.x) & is.null(pos.y)){
					p<-p+scale_fill_manual(values="#6495ED",guide = "none")
				}
			} else{
				p<-p+scale_fill_manual(values="#6495ED",guide = "none")
			}
	}

	#size
	if(!all(fixln(tmp.data$size)==0)){
		if(is.factor(tmp.data$size)){
			p<-p+scale_size_discrete(range = c(input$variable_size_min,input$variable_size_max)) # c(input$variable_size_min,input$variable_size_max)
		} else{
			p<-p+scale_size_continuous(range =c(input$variable_size_min,input$variable_size_max)) #  c(input$variable_size,input$variable_size)
		}	
	}
	
	#add facets
	p<-p + facet.type
	print(p)
}

#save plot
save.plot.name<-reactive(function(){
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
		make.ggplot()
		dev.off()
	}
)

#output dimensions
viz_plot_width <- function() {
        if(is.null(input$viz_plot_width)) return(650)
         input$viz_plot_width
}

viz_plot_height <- function() {
        if(is.null(input$viz_plot_height)) return(650)
        # if(input$viz_multiple) { 
                # nrPlots <- length(input$vizvars1)
                # ifelse(nrPlots > 1, return(325 * ceiling(nrPlots / 2)), return(650))
        # } else {
                 return(input$viz_plot_height)
        # }
}


#out put for Visualize
plot.visualize <- function(result){
	if(is.null(input$datasets)) return()
	if(input$analysistabs != 'Plots') return()
	
	make.ggplot()
	
}#, width = viz_plot_width, height = viz_plot_height)

summary.visualize <- function(result) {
	str(getdata())
}

visualize<-reactive({
return(list('plotHeight' =viz_plot_height(),'plotWidth' =viz_plot_width() ))
})
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

#group
output$group_var<-renderUI({
	if (is.null(getdata())){return()}
		var.opts<-varnames()#colnames(get(input$data))#
		selectInput("group_var", "",c( none="", var.opts),multiple = TRUE)
		# updateSelectInput(session, "variable", choices = var.opts)
	 })	

#map variable to size
output$size_variable<-renderUI({
				if (is.null(getdata())){return()}
				var.opts<-varnames()#colnames(get(input$data))#
				selectInput("size_variable", "", var.opts)
})

#size
size.ui<-renderUI({ 
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
ui_visualize <- renderUI( {

list(
	wellPanel(
	# downloadButton('Download_vis_plot', label = "Download plot"),
	# br(),
	# h4(''),
	tags$details(open="open",tags$summary("Plot"),	
			 selectInput(
			"plot_type", "Type:",
			c(
			"Bar plot" 		= "bar_plot",
			"Histogram" 	= "histogram",
			"Density plot" 	= "density_plot",
			"Box plot" 		= "box_plot",
			"Scatter plot" 	= "scatter_plot"),		
			selected  		= "Bar plot"
			 ),
			# interweaving UIs
			uiOutput("x_var"),
			conditionalPanel(
					condition = "input.plot_type == 'scatter_plot'",
					uiOutput("y_var")
				),
			#boxplot
			conditionalPanel(
					condition = "input.plot_type == 'box_plot'",		
					checkboxInput("show_points", "show points", TRUE)
			),
			conditionalPanel(
					condition = "input.plot_type == 'bar_plot'",		
					checkboxInput("sort_bar_plot", "sort", FALSE),
					checkboxInput("vertical_bar_plot", "vertical", FALSE)
			)		
		)
	),
	wellPanel(	
		tags$details(tags$summary("Color"),
					uiOutput("group_var"),
					uiOutput("alpha.ui")
			)
	),
	conditionalPanel(condition = "input.plot_type == 'scatter_plot'",
		wellPanel(	
		tags$details(tags$summary("Size"),
					size.ui()
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
				"polygon" 	= "polygon",
				"linear model" 		= "lm_mod",
				"loess smoother" 	= "loess_mod"),		
				selected  		= "none"
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
})
	
# # dataview visualizer
# output$ui_visualize <- renderUI({
	# ui_visualize()
# })
