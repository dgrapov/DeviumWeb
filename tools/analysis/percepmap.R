# variable selection - factor analysis bases perceptual map
output$pmap_brand <- renderUI({

	varCls <- getdata_class()
	isChar <- "character" == varCls
  vars <- varnames()[isChar]
  if(length(vars) == 0) return()
 	selectInput(inputId = "pmap_brand", label = "Brand:", choices = vars, selected = NULL, multiple = FALSE)
})

output$pmap_attr <- renderUI({
  if(is.null(input$pmap_brand) || is.null(inChecker(c(input$pmap_brand)))) return()

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "pmap_attr", label = "Attributes:", choices = vars, selected = NULL, multiple = TRUE)
})

output$pmap_pref <- renderUI({
  if(is.null(input$pmap_attr)) return()

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
 	vars <- vars[-which(vars %in% input$pmap_attr)]
  if(length(vars) == 0) return()

  selectInput(inputId = "pmap_pref", label = "Preferences:", choices = vars, selected = NULL, multiple = TRUE)
})

output$pmap_dim_number <- renderUI({
  radioButtons(inputId = "pmap_dim_number", label = "", c("2-dims" = 2, "3-dims" = 3), selected = "2-dims")
})

output$pmap_plot <- renderUI({
	plot_list <- c("Brands" = "brand", "Attributes" = "attr")
  if(!is.null(input$pmap_pref)) plot_list <- c(plot_list, c("Preferences" = "pref"))
	checkboxGroupInput("pmap_plot", "", plot_list)
})

ui_pmap <- function() {
  list(wellPanel(
  	uiOutput("pmap_brand"),
  	uiOutput("pmap_attr"),
  	uiOutput("pmap_pref"),
 	 	conditionalPanel(condition = "input.analysistabs == 'Plots'",
	  	uiOutput("pmap_plot"),
 	    div(class="row-fluid",
	    	div(class="span6", numericInput("pmap_scaling", "Arrow scaling:", 2.1, .5, 4, .1)),
	      div(class="span6", numericInput("pmap_fontsz", "Font size:", 1.3, .5, 4, .1))
	    )
    ),
  	uiOutput("pmap_dim_number"),
 	 	conditionalPanel(condition = "input.analysistabs == 'Summary'",
    	numericInput("pmap_cutoff", label = "Loadings cutoff:", min = 0, max = 1, value = 0, step = .05),
	    actionButton("pmap_savescores", "Save scores")
	  )
 		),
		helpModal('Attribute based maps','mds',includeHTML("tools/help/percepmap.html"))
	)
}

plot.pmap <- function(result) {

	out <- result$out

	std.pc <- input$pmap_scaling * out$pref.cor
	std.m <- input$pmap_scaling * out$loadings
	std.scores <- out$scores
	lab_buf <- 1.1
	max.max <- max(abs(std.m),abs(std.scores),abs(std.pc)) * 1.1	# adding a buffer so the labels don't move off the screen

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
		# op <- par(mfrow=c(1,1))
	} else {
		op <- par(mfrow=c(1,1))
	}

	for(i in 1:(out$nr.dim-1)) {
			for(j in (i+1):out$nr.dim) {

				# max.max <- max(abs(std.m[,c(i,j)])) * 1.1	# adding a buffer so the labels don't move off the screen

				plot(c(-max.max,max.max),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-max.max, max.max), xlim=c(-max.max,max.max))
				# title(paste("Perceptual Map for",input$datasets,"data\nDimension",i,"vs Dimsension",j))
				title(paste("Dimension",i,"vs Dimension",j), cex.main = input$pmap_fontsz)
				abline(v=0, h=0)

				if("brand" %in% input$pmap_plot) {
					points(std.scores[,i], std.scores[,j], pch = 16, cex = .6)
					# text(std.scores[,i], std.scores[,j], out$brand.names, col="darkgreen", cex = 1.2, adj = c(-.1,-.2))
					# textplot(std.scores[,i], std.scores[,j]*lab_buf, out$brand.names, cex = input$pmap_fontsz, col="darkgreen", new = FALSE)
					textplot(std.scores[,i], std.scores[,j]+(.04*max.max), out$brand.names, cex = input$pmap_fontsz, new = FALSE)
				}
			
				if("attr" %in% input$pmap_plot) {
					# text(lbf*std.m[,i], std.m[,j], input$pmap_attr, col="darkblue", cex = 1.2, adj=c(0.5,-.3))
					textplot(std.m[,i]*lab_buf, std.m[,j]*lab_buf, input$pmap_attr, cex = input$pmap_fontsz, col = "darkblue", new = FALSE)
					# add arrows
					for (k in input$pmap_attr) 
						# arrows(0,0, x1=std.m[k,i], y1=std.m[k,j], col="borange", cex=1, length=.1)
						arrows(0,0, x1=std.m[k,i], y1=std.m[k,j], lty='dashed', length=.05)

					# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_segment
					# http://docs.ggplot2.org/0.9.3.1/geom_abline.html
				}

				if("pref" %in% input$pmap_plot) {
					# text(lbf*std.pc[,i], std.pc[,j], input$pmap_pref, col="darkred", cex = 1.2,adj=c(0.5,-.3))
					textplot(std.pc[,i]*lab_buf, std.pc[,j]*lab_buf, input$pmap_pref, cex = input$pmap_fontsz, col="darkred", new = FALSE)
					for (l in input$pmap_pref) 
						arrows(0,0, x1=std.pc[l,i], y1=std.pc[l,j], lty='dashed', col="red", length=.05)
				}
			}
		}

	par(op)

}

pmap <- reactive({

	ret_text <- "This analysis requires a brand variables of type character and multiple attribute variables of type numeric or integer. Please select another dataset."
	if(is.null(input$pmap_brand)) return(ret_text)
	if(is.null(inChecker(c(input$pmap_brand, input$pmap_attr)))) return(ret_text)
	if(length(input$pmap_attr) < 2) return("Please select two or more attribute variables")

	dat <- getdata()

	nr.dim <- as.numeric(input$pmap_dim_number)
	nr.attr <- length(input$pmap_attr)
	# sf <- as.numeric(state$sf)

	f.data <- dat[,input$pmap_attr]
	brands <- dat[,input$pmap_brand]

	f.res <- suppressWarnings(principal(f.data, nfactors=nr.dim, rotate='varimax', scores=FALSE, oblique.scores=FALSE))
	df <- as.data.frame(f.res$loadings[,colnames(f.res$loadings)])
	m <- as.matrix(df)
	mPm <- (t(m) %*% m)
	smPm <- solve(mPm)
	cscm <- m %*% smPm
	mcar <- as.matrix(f.data)
	scores <- scale(mcar, center = TRUE, scale = TRUE) %*% cscm
	rownames(scores) <- brands

	pc <- std.pc <- 0
	if(!is.null(input$pmap_pref)) {
		pc <- cor(dat[,input$pmap_pref],scores)
		rownames(pc) <- input$pmap_pref
	}

	out <- list()
	out$nr.dim <- nr.dim
	out$scores <- scores
	out$loadings <- m
	out$pref.cor <- pc 
	out$brand.names <- brands
	out$brand.attr <- input$pmap_attr
	if(!is.null(input$pmap_pref))
		out$pref.names <- input$pmap_pref
	
	# nr.plots <- factorial(c(nr.dim,2))
	# plotHeight <- 650 * (nr.plots[1] / nr.plots[2])

	nr.plots <- (nr.dim * (nr.dim - 1)) / 2
	plotHeight <- 650 * nr.plots

	return(list('f.res' = f.res, 'out' = out, 'plotHeight' = plotHeight))
})

summary.pmap <- function(result) {

	cat("-- Attribute based perceptual map --\n")
	f.res <- result$f.res
	out <- result$out

	cat("\nBrand - Factor scores:\n")
	print(out$scores, digits = 3)

	cat("\nAttribute - Factor loadings:\n")
	# print(f.res$loadings, cutoff = 0)
	print(f.res$loadings, cutoff = input$pmap_cutoff, digits = 3)
	
	if(!is.null(input$pmap_pref)) {
		cat("\nPreference correlations:\n")
		print(out$pref.cor, digits = 3)
	}

	communalities <- as.data.frame(f.res$communality)
	colnames(communalities) <- "communalities"
	cat("\nAttribute communalities:\n")
	print(communalities, digits = 3)
}

# save factor scores when action button is pressed
observe({
	if(is.null(input$pmap_savescores) || input$pmap_savescores == 0) return()
	isolate({
		if(is.character(pmap())) return()
		facscores <- data.frame(pmap()$out[['scores']])
		changedata(facscores, paste0("fac",1:input$pmap_dim_number))
	})
})
