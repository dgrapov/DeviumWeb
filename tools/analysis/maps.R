# variable selection - MDS
output$mds_id1 <- renderUI({
	varCls <- getdata_class()
	isChar <- "character" == varCls
  vars <- varnames()[isChar]
  if(length(vars) == 0) return(HTML('<label>This dataset has no variables of type character.</label>'))
  # if(length(vars) == 0) return()
  selectInput(inputId = "mds_id1", label = "ID 1:", choices = vars, selected = NULL, multiple = FALSE)
})

output$mds_id2 <- renderUI({

  if(is.null(input$mds_id1) || !input$mds_id1 %in% varnames()) return()

  varCls <- getdata_class()
  isChar <- "character" == varCls
  vars <- varnames()[isChar]
	vars <- vars[-which(vars == input$mds_id1)]
  if(length(vars) == 0) return(HTML('<label>This dataset has only one variable of type character.</label>'))
  # if(length(vars) == 0) return()
  selectInput(inputId = "mds_id2", label = "ID 2:", choices = vars, selected = NULL, multiple = FALSE)
})

output$mds_dis <- renderUI({
  if(is.null(input$mds_id2) || !input$mds_id2 %in% varnames()) return()

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  # if(length(vars) == 0) return(HTML('<label>This dataset has no numeric variables.</label>'))
  if(length(vars) == 0) return()
  selectInput(inputId = "mds_dis", label = "Dissimilarity:", choices = vars, selected = NULL, multiple = FALSE)
})

output$mds_dim_number <- renderUI({
  radioButtons(inputId = "mds_dim_number", label = "", c("2-dims" = 2, "3-dims" = 3), selected = "2-dims")
})

output$mds_rev_dim <- renderUI({
	rev_list <- list()
	rev_list[paste("Dimension",1:input$mds_dim_number)] <- 1:input$mds_dim_number
	checkboxGroupInput("mds_rev_dim", "Reverse:", rev_list)
})

ui_mds <- function() {
  list(wellPanel(
  	uiOutput("mds_id1"),
  	uiOutput("mds_id2"),
  	uiOutput("mds_dis"),
  	uiOutput("mds_dim_number"),
 	 	conditionalPanel(condition = "input.analysistabs == 'Plots'",
 	 		numericInput("mds_fontsz", "Font size:", 1.3, .5, 4, .1),
	  	uiOutput("mds_rev_dim")
    )),
		helpModal('(Dis)similarity based maps (MDS)','mds',includeHTML("tools/help/mds.html"))
	)
}

summary.mds <- function(result) {

	cat("Distance data:\n")
	print(result$co.dist.mat, digits = 3)
	cat("\nCoordinates:\n")
	co.mds <- result$co.mds
	coor <- co.mds$points
	colnames(coor) <- paste("Dim ", 1:ncol(coor))
	print(coor, digits = 3)

	cat("\nFinal stress measure equal to", sprintf("%.3f", co.mds$stress/100))
}

plot.mds <- function(result) {

	out <- result$out

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
	} else {
		op <- par(mfrow=c(1,1))
	}

	if(!is.null(input$mds_rev_dim)) {
		dim2rev <- as.numeric(input$mds_rev_dim)
		out$points[,dim2rev] <- -1 * out$points[,dim2rev]
	}

	for(i in 1:(out$nr.dim-1)) {
		for(j in (i+1):out$nr.dim) {
			plot(c(-out$lim,out$lim),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-out$lim, out$lim), xlim=c(-out$lim,out$lim))
			title(paste("Dimension",i,"vs Dimension",j), cex.main = input$mds_fontsz)
			points(out$points[,i], out$points[,j], pch = 16, cex = .6)

			# text(out$points[,i], out$points[,j], out$labels, col=rainbow(out$nr.lev,start=.6,end=.1), adj = c(0.4,-.4))
			textplot(out$points[,i], out$points[,j]+(.04*out$lim), out$labels, col=rainbow(out$nr.lev,start=.6,end=.1), cex = input$mds_fontsz, new = FALSE)
			abline(v=0, h=0)
		}
	}

	par(op)


	###############################################
	# move this over to ggplot when you have time
	###############################################

	# plots <- list()
	# for(var in input$km_vars) {
	# 	plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
	# }

	# dat <- data.frame(out$points)
	# colnames(dat) <- paste0('Dimension',1:out$nr.dim)
	# dim1 <- 'Dimension1'
	# dim2 <- 'Dimension2'
	# p <- ggplot(dat, aes_string(x=dim1, y=dim2)) + geom_point(shape = 1) + 
	# 			theme(axis.text.x = element_blank(), axis.text.y = element_blank())
	# print(p)

  # p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
 	# p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
	# print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}

mds <- reactive({

	ret_text <- "This analysis requires two id-variables of type character and a measure of dissimilarity of type numeric or interval. Please select another dataset."
	if(is.null(input$mds_id2) || is.null(input$mds_dis)) return(ret_text)

	dat <- getdata()

	if(is.null(inChecker(c(input$mds_id1, input$mds_id2, input$mds_dis)))) return(ret_text)

	nr.dim <- as.numeric(input$mds_dim_number)

	dis <- dat[,input$mds_dis]
	id1 <- dat[,input$mds_id1]
	id2 <- dat[,input$mds_id2]

	lab <- unique(c(id1,id2))
	nr.lev <- length(lab)

	lower <- (nr.lev * (nr.lev -1)) / 2
	nr.obs <- length(dis)

	co.dist <- diag(length(lab))
	if(lower == nr.obs) { 
		co.dist[lower.tri(co.dist, diag = FALSE)] <- dis 
	} else if((lower + nr.lev) == nr.obs) { 
		co.dist[lower.tri(co.dist, diag = TRUE)] <- dis 
	} else { 
		return("Number of observations and unique id's does not match.")
	}

	rownames(co.dist) <- lab
	colnames(co.dist) <- lab
	co.dist.mat <- as.dist(co.dist)

	set.seed(1234)

	###############################################
	# Might try metaMDS at some point
	###############################################
	
	# co.mds <- suppressWarnings(metaMDS(co.dist.mat, k = nr.dim, trymax = 500))
	# if(co.mds$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

	co.mds <- isoMDS(co.dist.mat, k = nr.dim, trace = FALSE)

	out <- list()
	out$nr.dim <- nr.dim
	out$data <- co.dist.mat
	out$points <- co.mds$points
	out$labels <- lab
	out$nr.levels <- nr.lev
	out$lim <- max(abs(out$points))

	# nr.plots <- factorial(c(nr.dim,2))
	# plotHeight <- 650 * (nr.plots[1] / nr.plots[2])

	nr.plots <- (nr.dim * (nr.dim - 1)) / 2
	plotHeight <- 650 * nr.plots

	return(list('co.mds' = co.mds, 'co.dist.mat' = co.dist.mat, 'out' = out, 'plotHeight' = plotHeight))
})
