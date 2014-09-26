
#fix:
# add ability to resize plots

#tests
test<-function(){
data(mtcars)
data<-mtcars
clusters<-hclust(dist(data))
			A2Rplot(clusters, k = 3, boxes = FALSE, col.up = "gray50", col.down = rainbow(3), main="")
			ccolor<-rev(rainbow(input$hc_nrClus))
			legend("topright", legend=paste("cluster",1:input$hc_nrClus, sep=" "), fill=ccolor, border= ccolor,bty = "n" )
}

#select colors for heatmap options
color.opts<-function(){c("red","orange","yellow","green","blue","violet","purple","white","black","gray")}

#fix numeric rownames (sometimes no effect try other methods inside)
# need to append numeric rownames with X (and avoid error causing characters in names) to stay consistent when matrix transposed
rdy.t<-function(obj){
  list<-dimnames(obj)
  names<-lapply(seq(list), function(i){
    tmp<-check.fix.names(fixlc(list[[i]]),ok.chars=c(".","_"))
    test<-!is.na(as.numeric(tmp))
    paste(ifelse(test,"X",""),tmp,sep="")           
  })
  out<-as.data.frame(obj)
  dimnames(out)<-names
  return(data.frame(out))
}

# HCA UI
#--------------------------------------
# variable selection - hclustering
output$hc_vars <- renderUI({
  vars <- varnames2()
  if(is.null(vars)) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = names(vars), multiple = TRUE,selectize=FALSE)
})

# annotation variable selection - hclustering
output$hc_group_vars <- renderUI({

	# tmp.data <- getdata2()[,input$hc_vars,drop=FALSE]
	tmp.data <- getdata2()
	if(is.null(tmp.data)) {return()}
	
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    #tmp.data[is.na(tmp.data)]<-0 # ignore NA for now
    
    #remove factors else could convert to numerics
	# should go into annotation section
    # fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
    # tmp.data<-tmp.data[,!fct.id, drop=F]
    
    if(input$dimention=="1"){
      tmp.data<-data.frame(t(data.frame(tmp.data)))
    } 

	# if(input$dimention=="1"){
		# group.opts<-colnames(tmp.data)
    # } else {
		group.opts<-rownames(tmp.data)
    # }
	
  vars <-group.opts
  if(is.null(vars)) return()
  selectInput(inputId = "hc_group_vars", label = "Annotation:", choices = c("none",vars), selected ="none", multiple = TRUE)
})

#number of clusters
# #condition slider max on dim[2]				
output$hc_clusters <- renderUI({
   
    tmp.data <- getdata2()[,input$hc_vars,drop=FALSE]
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    #tmp.data[is.na(tmp.data)]<-0 # ignore NA for now
    
    #remove factors else could convert to numerics
	# should go into annotation section
    fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
    tmp.data<-tmp.data[,!fct.id, drop=F]
    
    if(input$dimention=="1"){
      tmp.data<-data.frame(t(data.frame(tmp.data)))
    } 
  
    ymax <-ncol(tmp.data) # 
    nclust<-tryCatch(input$HCA_groups, error= function(e) {NULL})
    # avoiding errors in stats
    if(is.null(nclust)){nclust<-3}
    if(nclust < 2) {nclust<-3}
    if(nclust>ymax) {nclust<-ymax-1}
    
	selectInput(inputId = "hc_nrClus", label = "Number of clusters", choices = 3:ymax, selected = nclust, multiple = FALSE)
    # sliderInput(inputId = "hc_nrClus","Number of Clusters",min= 3, max = ymax, value = nclust, step = 1)
  })
 
#method options
hc_transformation<-list("none" = "none", "Z-scale"= "z.scale", "Spearmans correlation" = "spearman", "Pearsons correlation" = "pearson","Biweight correlation" = "biweight")
hc_method <-  c("none","ward", "single", "complete", "average", "mcquitty", "median" , "centroid")
hc_dist_method <-  c("none","euclidean", "maximum", "manhattan", "canberra", "binary" ,"minkowski")

# #caption
# output$caption<-renderText({c(input$hc_group_vars)})

#plot type
plot_control_hclustering<-function(){
	tags$details(
		radioButtons(inputId = "hclustering_plot_heatmap", label = "Plot type:", choices = c("heatmap","dendrogram"), selected = "heatmap"),
	)		
} 

#plot dimensions
clustering_plot_width<-reactive({
	if(!input$tool=="hclustering") return()
	if(is.null(input$hclustering_plot_width))  return()
	values$plot$'plotWidth'<-input$hclustering_plot_width
})

clustering_plot_height<-reactive({
	if(!input$tool=="hclustering") return()
	if(is.null(input$hclustering_plot_height))  return()
	values$plot$'plotHeight'<-input$hclustering_plot_height
})

#watcher for plot dimensions
observe({
	if(!input$tool=="hclustering") return()
	clustering_plot_height()
	clustering_plot_height()
})

ui_hclustering <- function() {
  list(
	  wellPanel(
		# conditionalPanel(condition = "input.analysistabs=='Plots'",downloadButton('Download_cluster_plot', label = "Download plot")),
		# br(),
			h4("Data"),
			tags$details(open="open", tags$summary("Options"),	
				radioButtons("dimention","Dimension:", list("rows" = 1,"columns" = 2), selected = "rows"),
				uiOutput("hc_vars"), # data variables
			tags$style(type='text/css', "#hc_vars { height: 200px; padding-bottom: 10px;}"),
			 # flipping data rows/columns
			 br(),
			selectInput("hc_transformation", label = "Transformation:", choices = hc_transformation, selected = hc_transformation[1], multiple = FALSE)
			)
		),
		wellPanel(
			h4("Cluster"),
			tags$details(tags$summary("Options"),
				selectInput("hc_dist", label = "Distance measure:", choices = hc_dist_method, selected = "euclidean", multiple = FALSE),
				selectInput("hc_meth", label = "Method:", choices = hc_method, selected = "ward" , multiple = FALSE),
				uiOutput("hc_clusters"),
				actionButton("hc_saveclus", "Save cluster membership")
			)
		),
		wellPanel(
			h4("Plot"),
			tags$details(tags$summary("Type"),
				radioButtons(inputId = "hclustering_plot_heatmap", label = "Plot type:", choices = c("heatmap","dendrogram"), selected = "heatmap"),
				conditionalPanel(condition = "input.dimention=='1'",
					uiOutput("hc_group_vars")
				),
				checkboxInput("names","Names",TRUE),
				checkboxInput("border","Border",FALSE)
			),	
			tags$details(tags$summary("Color"),
				selectInput("low_col","low", color.opts(), selected = "green" ),
				selectInput("mid_col","mid", color.opts(), selected = "black"),
				selectInput("high_col","high", color.opts(),selected = "red")
			),
		tags$details(tags$summary("More options"),
			numericInput(inputId ="hclustering_plot_text_size", label = "Text size:", value=14, min = 0, step = 1),
			div(class="row-fluid", # see css.style for this as well
					div(class="span6",numericInput("hclustering_plot_width", label = "width", min = 0, step = 50, value = 650)),
					div(class="span6", numericInput("hclustering_plot_height", label = "height", min = 0, step = 50, value = 650))
			),	
			tags$style(type="text/css", "#hclustering_plot_width {width:75px;}"),
			tags$style(type="text/css", "#hclustering_plot_height {width:75px;}"),
			br(),
			downloadButton('Download_cluster_plot', label = "Download plot")
			)	
		)#,
		# wellPanel(
			# tags$details(tags$summary("More options"),
				# checkboxInput("calculate_edge_list","Calculate edge list?",FALSE)
			# )
		# )
	)
}
  
# #make heatmap
plot.hclustering <- function(result){ # not sure why result is necessary
   
   if(!input$analysistabs == 'Plots') return()
	
	if (input$hclustering_plot_heatmap=="heatmap"){
		result<-values$clustering.results.object # to save plots
		#plot heatmap 
		devium.heatmap(
		  data 			      	= result$data, 
		  match.dim 		 	= result$match.dim,
		  type 			     	= result$type,
		  class.factor 	  		= result$class.factor,
		  class.color 	  		= NULL,
		  heatmap.color	 	 	= result$heatmap.color,
		  cluster.method 		= result$cluster.method, 
		  distance.method 		= result$distance.method,
		  font.size 		  	= result$hclustering_plot_text_size,
		  border.color	  		= result$border.color,
		  show.names 		  	= result$show.names)
	}
	
	if (input$hclustering_plot_heatmap=="dendrogram"){
		
		tmp<-hclustering()
		if (!tmp$distance.method == "none" | !tmp$cluster.method == "none") {
			clusters<-hclustering_results()
			k.clust<-as.numeric(input$hc_nrClus)
			devium.dendrogram(clusters, k = k.clust, boxes = FALSE, col.up = "gray50", col.down = rainbow(k.clust), main="",text.cex=result$hclustering_plot_text_size)
			ccolor<-rev(rainbow(input$hc_nrClus))
			legend("topright", legend=paste("cluster",1:input$hc_nrClus, sep=" "), fill=ccolor, border= ccolor,bty = "n" )
		} else {
			empty.plot("Please calculate clusters before plotting.")
		}
	}
	
  }#)

#save plot
save.plot.cluster.name<-reactive(function(){
	# paste(input$datasets,input$plot_type,sep="-")
	sprintf("%s_%s.%s",input$datasets,input$hclustering_plot_heatmap,'pdf')
	})

output$Download_cluster_plot<-downloadHandler(
      filename = save.plot.cluster.name,
    # a function that actually opens a connection to a pdf and print the result to it
    content = function(FILE=NULL) {
	
		# make.ggplot()
		# ggsave(file="plot.pdf")#FILE
		
	    pdf(file=FILE, onefile=T, width=12,height=8)
		# print(make_PC_Plot())
		plot.hclustering()
		dev.off()
	}
)  
   
# # #----------output-----------------------
summary.hclustering <- function(result) {
	
	if(is.null(varnames())) return()
	
	cluster.id<-grepl("hclus",varnames()) # ?
	cluster.data<-getdata2()[,cluster.id,drop=FALSE] #cluster.id
	
	if(!is.null(values$col_hc_clusters)){#TODO: add reset mechanism elsewhere hack for now!!!!
		# if(nrow(values$col_hc_clusters)>nrow(getdata())) values$col_hc_clusters<-NULL
	} 

	list(sample.cluster.info = cluster.data,
		variable.cluster.info = values$col_hc_clusters) #, edge.list = hc_edge.list
}

# place all reactive inside here the result is sent to plots or summaries
hclustering<-reactive({ # clustering output
	
	tmp<-list() # collect args to pass to plot
	#dimension
	tmp$match.dim<- as.numeric(input$dimention)
	
	#data
    tmp.data <-getdata2()[,input$hc_vars,drop=FALSE]
    colnames(tmp.data)<-check.fix.names(colnames(tmp.data),ok.chars=c(".","_") )
    rownames(tmp.data)<-check.fix.names(rownames(tmp.data),ok.chars=c(".","_") )
    fct.id<-sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
	tmp.vars<-getdata2() # save annotation
    # tmp.data<-tmp.data[,!fct.id, drop=F] # remove factors, convert to numeric instead
    
    # if(input$dimention=="1"){
      # tmp.data<-data.frame(t(data.frame(tmp.data)))
    # } 
    tmp$data<-afixln(tmp.data) #make all numeric
	
	#top row(s) annotation
	if(input$hc_group_vars=="none"){
			class.factor<-NULL 
		} else {
			if(tmp$match.dim==1){# mind f$ck for flipping
				class.factor<-as.data.frame(tmp.vars[,colnames(tmp.vars)%in%input$hc_group_vars,drop=F]) 
			} else {
				class.factor<-t(as.data.frame(tmp.vars[rownames(tmp.vars)%in%input$hc_group_vars,,drop=F]))# [,!fct.id,drop=F])
			}
	}
	tmp$class.factor<-class.factor
	tmp$type <-input$type
    tmp$show.names <-input$names
    tmp$border.color <-ifelse(input$border==TRUE,"gray40",FALSE)
	if(input$hc_meth == "none"|input$hc_dist == "none"){
		tmp$cluster.method<- "none"
		tmp$distance.method<-"none"
    } else {
		tmp$cluster.method <- input$hc_meth
		tmp$distance.method <- input$hc_dist
    }
	tmp$type<-input$hc_transformation
	tmp$hclustering_plot_text_size<-input$hclustering_plot_text_size
	tmp$heatmap.color<-c(input$low_col,input$mid_col,input$high_col)
	values$clustering.results.object<-tmp
	return(tmp)
})


# # this is calculated inside the plot already 
# # would need to hack pheatmap fxn to avoid double calcs
hclustering_results <- reactive({ # doing the clustering
	
	if(is.null(input$hc_vars)) return(NULL)
		if(input$hc_dist =="none" | input$hc_meth =="none") return(NULL)
	
		tmp<-hclustering()
		#make sure clustering matches what heatmap does
		#prepare data object
		if(tmp$match.dim==1){tmp.data<-data.frame(t(data.frame(tmp$data)))} else {tmp.data<-data.frame(tmp$data)}

		
		if(tmp$type == "z.scale") {tmp.data<-scale(tmp.data, center=TRUE, scale = TRUE)}		

		# calculate correlations
		if(!tmp$type=="none" & !tmp$type == "z.scale" ){
			cor<-devium.calculate.correlations(tmp.data,type=tmp$type)
			tmp.data<-cor$cor
			tmp.data.pvalue<-cor$p.value
			
			#create edge list for storage of correlation info
			# replace with faster adjacency to edgelist from "network" as.matrix.network.edgelist
			# currently a bit too slow for large data
			# if(input$calculate_edge_list) {
				# cor<-gen.mat.to.edge.list(tmp.data)
				# p.vals<-gen.mat.to.edge.list(tmp.data.pvalue)
				# adj.p<-p.adjust(fixln(p.vals[,3]), method="BH")
				# adj.p[is.na(as.numeric(adj.p))]<- 0 # error vars, assume due cor =1
				# values$hclust_edge_list<-data.frame(cor,p.values = p.vals[,3], adjusted_p.values = adj.p)
			# }
			
			if(is.numeric(tmp$alpha)){ # not used
			  tmp.data[tmp.data>0]<-1
			  tmp.data[tmp.data<0]<--1
			  tmp.data[tmp.data.pvalue>tmp$alpha]<-0
			  ncolors<-3 # limit heat map to 3 colors
			}
		}
	
		#if(!tmp$cluster.method=="none"){
		cluster_rows<-cluster_cols<-T
		# calculate distances
		#if(!tmp$distance.method=="none"){
			                 				
				clustering_distance_rows<-dist(tmp.data, method= tmp$distance.method)
				clustering_distance_cols<-dist(data.frame(t(tmp.data)), method= tmp$distance.method)
				if(!is.null(tmp$type)){
					if(!tmp$type=="none" & !tmp$type == "z.scale"){ # reverse logic throws errors in shiny?
						tmp.data.dist<-dist(tmp.data, method= tmp$distance.method) # 
						clustering_distance_cols<-clustering_distance_rows<-tmp.data.dist
					}
				} 			
		
		hclust(d = clustering_distance_cols, method= tmp$cluster.method)
})

#storage for variable clusters
var.clusters<-function(){
	if(is.null(values$col_hc_clusters)) {
		# tmp.data <-getdata2() #remove factors
		# fct.id<-!sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
		# values$col_hc_clusters<-data.frame(id=matrix(c(1:sum(fct.id)),ncol=1))
	} else {
		values$col_hc_clusters
	}
}

observe({ # adding cluster membership to the data
	if(is.null(input$hc_saveclus) || input$hc_saveclus == 0) return()
	isolate({
		tmp<-hclustering()
		if(tmp$cluster.method =="none" | tmp$cluster.method == "none") return()
		clusmem <- tryCatch(cutree(hclustering_results(), k = as.numeric(input$hc_nrClus)), error=function(e){rep("error",length(hclustering_results()$order))})
		
		# data transpose should be handled centrally and only items should be bound as columns (need to redo whole design!)
		# 
		if(tmp$match.dim == 1 ) {
			#bind with data 
				changedata(data.frame(as.matrix(as.factor(clusmem))), paste("hclus",input$hc_nrClus,sep=""))
			
		} else {
			name<-paste0(input$datasets,"_HCA_variable_info")
			clus.name<-paste("hclus",input$hc_nrClus,sep="")
			assign(clus.name,as.factor(clusmem))
			tmp<-values$clustering.results.object$data
			id<-na.omit(match(colnames(getdata()),colnames(tmp)))
			tmp.obj<-values$col_hc_clusters<-data.frame(index=id,names=colnames(tmp),clusmem)
			values[[name]]<-tmp.obj
			values$datasetlist <- unique(c(values$datasetlist,name))
			 # # save variable info but don't bind with data
				# addColName<-paste("hclus",input$hc_nrClus,sep="")
				# tmp<-var.clusters()
				# var.clus.info<-data.frame(do.call("cbind",values$col_hc_clusters))
				# tmp.data <-getdata2() #remove factors
				# fct.id<-!sapply(seq(tmp.data), function(i){is.factor(tmp.data[,i])})
				# var.clus.info[,1]<-colnames(tmp.data)[fct.id]
				# var.clus.info[,addColName]<-clusmem
				# values$variable_heirarchical_clusters<-values$col_hc_clusters<-var.clus.info
				# values$datasetlist <- unique(c(values$datasetlist,"variable_heirarchical_clusters"))
			
		}
		
			
		
	})
})



#------------------------------------ from radiant currently disabled
# K-means objects
#--------------------------
#additional controls for plots
plot_control.kmeansClustering<-function(){NULL}

output$km_vars <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})
  
ui_kmeansClustering <- function() {
  wellPanel(
    uiOutput("km_vars"), 
	  checkboxInput(inputId = "km_hcinit", label = "Initial centers from HC", value = TRUE),
  	conditionalPanel(condition = "input.km_hcinit == true",
  		wellPanel(
	  		selectInput("km_dist", label = "Distance measure:", choices = hc_dist_method, selected = hc_dist_method[1], multiple = FALSE),
  			selectInput("km_meth", label = "Method:", choices = hc_method, selected = hc_method[1], multiple = FALSE)
  		)
  	),
  	conditionalPanel(condition = "input.km_hcinit == false", 
	    numericInput("km_seed", "Set random seed:", 1234, min = 0)
	  ),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership")
  )
}

summary.kmeansClustering <- function(result) {
	result$cluster = NULL
	result
}

plot.kmeansClustering <- function(result) {
	# several things to work on here to clean-up the plots
	dat <- getdata()[,input$km_vars, drop = FALSE]
	# gg.xlim <- quantile(as.vector(as.matrix(dat)),probs = c(.01,.99))
	dat$clusvar <- as.factor(result$cluster)

		plots <- list()
		for(var in input$km_vars) {
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + xlim(gg.xlim[1],gg.xlim[2]) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			# plots[[var]] <- ggplot(dat, aes_string(x=var, colour='clusvar')) + geom_density(adjust = 2) + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
			plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
		}
		print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}

hinitclustering <- reactive({
	if(is.null(input$km_vars)) return("Please select one or more variables")
	dat <- getdata()[,input$km_vars]
	if(input$km_dist == "sq.euclidian") {
		dist.data <- dist(dat, method = "euclidean")^2
	} else {
		dist.data <- dist(dat, method = input$km_dist)
	}
	hclust(d = dist.data, method= input$km_meth)
})

kmeansClustering <- reactive({
	if(is.null(input$km_vars)) return("Please select one or more variables")
	set.seed(input$km_seed)
	dat <- getdata()[,input$km_vars]
	# dat <- lapply(dat,as.numeric) 	# has strange effect
	if(input$km_hcinit) {
		clusmem <- cutree(hinitclustering(), k = input$km_nrClus)
		cluscenters <- as.matrix(aggregate(dat,list(clusmem),mean)[-1])
		kmeans(na.omit(object = data.frame(dat)), centers = cluscenters, iter.max = 500)
	} else {
		kmeans(na.omit(object = data.frame(dat)), centers = input$km_nrClus, nstart = 10, iter.max = 500)
	}
})

observe({
	if(is.null(input$km_saveclus) || input$km_saveclus == 0) return()
	isolate({
		clusmem <- kmeansClustering()$cluster
		changedata(as.factor(clusmem), paste("kclus",input$km_nrClus,sep=""))
	})
})
