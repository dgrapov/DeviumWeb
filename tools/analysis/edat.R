###############################
# Single mean
###############################

output$sm_var <- renderUI({
  vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- vars[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "sm_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

# for alternative hypothesis
alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

ui_singleMean <- function() {
  list(
  	wellPanel(
 	   	uiOutput("sm_var"),
  	  selectInput(inputId = "sm_alternative", label = "Alternative hypothesis:", choices = alt, selected = "Two sided"),
    	sliderInput('sm_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
    	numericInput("sm_compValue", "Comparison value:", 0)
  	),
 		# helpModal('Single mean','singleMean',includeMarkdown("tools/help/singleMean.md"))
 		helpModal('Single mean','singleMean',includeHTML("tools/help/singleMean.html"))
 	)
}

summary.singleMean <- function(result) {
	result
}

plot.singleMean <- function(result) {

	dat <- getdata()
	xvar <- dat[,input$sm_var]
	bw <- diff(range(xvar, na.rm = TRUE)) / 12

	p <- ggplot(dat, aes_string(x=input$sm_var)) + 
		geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) + 
		geom_vline(xintercept = input$sm_compValue, color = 'red', linetype = 'longdash', size = 1) +
		geom_vline(xintercept = mean(xvar, na.rm = TRUE), color = 'black', linetype = 'solid', size = 1) +
		geom_vline(xintercept = result$conf.int, color = 'black', linetype = 'longdash', size = .5)
	print(p)
}

singleMean <- reactive({

	ret_text <- "This analysis requires a variable of type numeric or interval. Please select another dataset."
	if(is.null(input$sm_var)) return(ret_text)
	if(is.null(inChecker(c(input$sm_var)))) return(ret_text)

	dat <- getdata()[,input$sm_var]
	t.test(dat, mu = input$sm_compValue, alternative = input$sm_alternative, conf.level = input$sm_sigLevel)
})

###############################
# Compare means
###############################

output$cm_var1 <- renderUI({

  varCls <- getdata_class()
	isNumOrFct <- "numeric" == varCls | "integer" == varCls | "factor" == varCls
  vars <- varnames()[isNumOrFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "cm_var1", label = "Select a factor or numerical variable:", choices = vars, selected = NULL, multiple = FALSE)
})

output$cm_var2 <- renderUI({

  if(is.null(input$cm_var1)) return()

  varCls <- getdata_class()
	isNum <- "numeric" == varCls | "integer" == varCls
  vars <- varnames()[isNum]
  if(length(vars) == 0) return()
 	if(input$cm_var1 %in% vars) {
	 	vars <- vars[-which(vars == input$cm_var1)]
	  if(length(vars) == 0) return()
	  selectInput(inputId = "cm_var2", label = "Variables (select one or more):", choices = vars, selected = NULL, multiple = TRUE)
	} else {
	  selectInput(inputId = "cm_var2", label = "Variables (select one):", choices = vars, selected = NULL, multiple = FALSE)
	}
})


ui_compareMeans <- function() {
  list(wellPanel(
    uiOutput("cm_var1"),
    uiOutput("cm_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
      selectInput(inputId = "cm_alternative", label = "Alternative hypothesis:", choices = alt, selected = "Two sided")
	    # radioButtons(inputId = "cm_paired", label = "", c("Paired" = "paired", "Independent" = "indep"), selected = "Paired")
    ),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
		  checkboxInput('cm_jitter', 'Jitter', value = TRUE)
		)),
  	helpModal('Compare means','compareMeans',includeHTML("tools/help/compareMeans.html"))
  )
}

summary.compareMeans <- function(result) {

	cat("\nMeans table:\n")
	means_tab <- ddply(result$data, c("variable"), colwise(mean))
	colnames(means_tab) <- c("","mean")
	print(means_tab, row.names = FALSE, right = FALSE)

	if(input$cm_alternative == "two.sided") {
		h.sym <- "not equal to" 
	} else if(input$cm_alternative == "less") {
		h.sym <- "<" 
	} else {
		h.sym <- ">" 
	}

	cat("\n\nPairwise comparisons using t-tests (bonferroni adjustment)\n\n")

	mod <- result[['pwcomp']]$p.value
	dvar <- dimnames(mod)
	var1 <- dvar[[1]] 
	var2 <- dvar[[2]] 

	res <- data.frame(matrix(ncol = 3, nrow = length(var1)*length(var1)/2))
	colnames(res) <- c("Alternative hyp.", "Null hyp.", "p-value")

	rnr <- 1
	for(i in var1) {
		for(j in var2) {
			if(is.na(mod[i,j])) next
			res[rnr, 'Alternative hyp.'] <- paste(i, h.sym, j,"     ") 
			res[rnr, 'Null hyp.'] <- paste(i, "=", j, "     ") 
			if(mod[i,j] < .001) { 
				pval = "< 0.001"
			} else {
				pval <- sprintf("%.3f", mod[i,j])
			}
			res[rnr, 'p-value'] <- pval
			rnr <- rnr + 1
		}
	}

	print(res, row.names = FALSE, right = FALSE)
}

plot.compareMeans <- function(result) {

	dat <- result$data
	var1 <- colnames(dat)[1]
	var2 <- colnames(dat)[-1]

	plots <- list()
	p <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + geom_boxplot(alpha=.3) 
	if(input$cm_jitter)	p <- p + geom_jitter() 
	plots[["Boxplot"]] <- p
	plots[["Density"]] <- ggplot(dat, aes_string(x=var2, fill=var1)) + geom_density(alpha=.3)

	print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

compareMeans <- reactive({

	ret_text <- "This analysis requires variables of type factor, numeric or interval. Please select another dataset."
	if(is.null(input$cm_var1)) return(ret_text)
	if(is.null(input$cm_var2)) return("Please select a numeric or interval variable")

	if(is.null(inChecker(c(input$cm_var1, input$cm_var2)))) return(ret_text)

	var1 <- input$cm_var1
	var2 <- input$cm_var2

	# dat <- na.omit( getdata()[,c(var1,var2)] )
	dat <- getdata()[,c(var1,var2)]

	if(!is.factor(dat[,var1])) {
    # updateRadioButtons(session = session, inputId = "cm_paired", label = "", c("Paired" = "paired", "Independent" = "indep"), selected = "Paired")
		cm_paired <- TRUE
		dat <- melt(dat)
		var1 <- colnames(dat)[1]
		var2 <- colnames(dat)[2]
	} else {
    # updateRadioButtons(session = session, inputId = "cm_paired", label = "", c("Paired" = "paired", "Independent" = "indep"), selected = "Independent")
		cm_paired <- FALSE
		colnames(dat)[1] <- "variable"
	}

	# if(input$cm_paired == 'paired') {
	if(cm_paired) {
		pwcomp <- with(dat,pairwise.t.test(get(var2), get('variable'), pool.sd = FALSE, 
			p.adj = "bonf", paired = TRUE, alternative = input$cm_alternative))
	} else {
		pwcomp <- with(dat,pairwise.t.test(get(var2), get('variable'), pool.sd = FALSE, 
			p.adj = "bonf", paired = FALSE, alternative = input$cm_alternative))
	}

	list("pwcomp" = pwcomp, "data" = data.frame(dat))
})

###############################
# Single proportion
###############################

output$sp_var <- renderUI({
  vars <- varnames()
  isFct <- "factor" == getdata_class()
 	vars <- vars[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "sp_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

ui_singleProp <- function() {
  list(wellPanel(
    uiOutput("sp_var"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    selectInput(inputId = "sp_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
  	  sliderInput('sp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
    	numericInput("sp_compValue", "Comparison value:", 0.5, min = 0.01, max = 0.99, step = 0.01)
    )),
	 	helpModal('Single proportion','singleProp',includeHTML("tools/help/singleProp.html"))
  )
}

summary.singleProp <- function(result) {
	result
}

plot.singleProp <- function(result) {

	var <- input$sp_var
	dat <- getdata()[,var, drop = FALSE]
	p <- ggplot(dat, aes_string(x = var, fill = var)) + geom_histogram(alpha=.3) +
 			ggtitle(paste("Single proportion:", var))
	print(p)
}

singleProp <- reactive({

	ret_text <- "This analysis requires a variable of type factor with two levels. Please select another dataset."
	if(is.null(input$sp_var)) return(ret_text)
	if(is.null(inChecker(c(input$sp_var)))) return(ret_text)

	dat <- getdata()[,input$sp_var]
	lev <- levels(dat)
	if(length(lev) >2) return("The selected variable has more than two levels. Try another variable or a cross-tab.")
	prop <- sum(dat == rev(lev)[1])
	prop.test(prop, n = length(dat), p = input$sp_compValue, 
		alternative = input$sp_alternative, conf.level = input$sp_sigLevel, correct = FALSE)
})

###############################
# Compare proportions
###############################

output$cp_var1 <- renderUI({
 	varCls <- getdata_class()
	isFct <- "factor" == varCls
  vars <- varnames()[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "cp_var1", label = "Select a grouping factor:", choices = vars, selected = NULL, multiple = FALSE)
})

output$cp_var2 <- renderUI({

	if(is.null(input$cp_var1)) return()
  varCls <- getdata_class()
	isFct <- "factor" == varCls
  vars <- varnames()[isFct]
	if(!input$cp_var1 %in% vars) return()
	vars <- vars[-which(vars == input$cp_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "cp_var2", label = "Select a 2-level factor:", choices = vars, selected = NULL, multiple = FALSE)
})

ui_compareProps <- function() {
  list(wellPanel(
    uiOutput("cp_var1"),
    uiOutput("cp_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
      selectInput(inputId = "cp_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
      numericInput('cp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )),
	 	helpModal('Compare proportions','compareProps',includeHTML("tools/help/compareProps.html"))
 	)
}

summary.compareProps <- function(result) {
	print(result$test)
	prop.table(result$tab, 1)
}

plot.compareProps <- function(result) {

	dat <- getdata()[,c(input$cp_var1,input$cp_var2)]
	p <- ggplot(dat, aes_string(x = input$cp_var1, fill = input$cp_var2)) + geom_bar(alpha=.3, position = "fill") +
				labs(list(title = paste("Comparing proportions of ",input$cp_var2,"$",levels(dat[,1])[1], " across levels of ",input$cp_var1, sep = ""), 
					x = paste("Factor levels for ", input$cp_var1), y = "Count", fill = input$cp_var2))

	print(p)
}

compareProps <- reactive({

	ret_text <- "This analysis requires variables of type factor. Please select another dataset."
	if(is.null(input$cp_var1) || is.null(input$cp_var2)) return(ret_text)
	if(is.null(inChecker(c(input$cp_var1, input$cp_var2)))) return(ret_text)

	var1 <- input$cp_var1
	var2 <- input$cp_var2

	dat <- getdata()[,c(var1,var2)]
	lev1 <- levels(dat[,1])
	lev2 <- levels(dat[,2])
	if(length(lev2) >2) return("The selected variable has more than two levels. Try another variable or a cross-tab.")

	tab <- table(group = dat[,input$cp_var1], variable = dat[,input$cp_var2])
	pt <- prop.test(tab, correct = FALSE, alternative = input$cp_alternative, conf.level = input$cp_sigLevel)
	pt$data.name <- paste("Group = ",var1,", variable = ",var2, " (level ", levels(dat[,var2])[1],")",sep = "")
	names(pt$estimate) <-  paste(paste("P(",var2,"$",lev2[1],")|",var1, sep = ""),"$",lev1, sep = "")
	list('test' = pt, 'table' = tab)
})

###############################
# Cross-tabs
###############################

output$ct_var1 <- renderUI({
 	varCls <- getdata_class()
	isFct <- "factor" == varCls
  vars <- varnames()[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "ct_var1", label = "Select a grouping factor:", choices = vars, selected = NULL, multiple = FALSE)
})

output$ct_var2 <- renderUI({

	if(is.null(input$ct_var1)) return()
  varCls <- getdata_class()
	isFct <- "factor" == varCls
  vars <- varnames()[isFct]
	if(!input$ct_var1 %in% vars) return()
	vars <- vars[-which(vars == input$ct_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "ct_var2", label = "Select a factor:", choices = vars, selected = NULL, multiple = FALSE)
})

ui_crosstab <- function() {
  list(wellPanel(
    uiOutput("ct_var1"),
    uiOutput("ct_var2"),
	  checkboxInput("ct_std_residuals", label = "Deviation (standarized)", value = FALSE),
	  checkboxInput("ct_deviation", label = "Deviation (percentage)", value = FALSE),
	  checkboxInput("ct_expected", label = "Expected values", value = FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
		  checkboxInput("ct_contrib", label = "Contribution to chisquare value", value = FALSE)
		  # checkboxInput("ct_rowperc", label = "Row percentages", value = FALSE),
		  # checkboxInput("ct_colperc", label = "Column percentages", value = FALSE),
		  # checkboxInput("ct_cellperc", label = "Cell percentages", value = FALSE)
		)),
	 	# helpModal('Cross-tabs','crossTabs',includeMarkdown("tools/help/crossTabs.md"))
	 	helpModal('Cross-tabs','crossTabs',includeHTML("tools/help/crossTabs.html"))
  )
}

summary.crosstab <- function(result) {
	cat("Observed values:\n")
	print(result$cst$observed)

	if(input$ct_std_residuals) {
		cat("\nDeviation (standardized):\n")
		print(result$cst$residuals, digits = 2) 	# these seem to be the correct std.residuals
	}
	if(input$ct_deviation) {
		cat("\nDeviation (percentage):\n")
		print(result$cst$deviation, digits = 2) 	# % deviation
	}
	if(input$ct_expected) {
		cat("\nExpected values:\n")
		print(result$cst$expected, digits = 2)
	}
	if(input$ct_contrib) {
		cat("\nContribution to chisquare value:\n")
		print((result$cst$observed - result$cst$expected)^2 / result$cst$expected, digits = 2)
	}
	# if(input$ct_cellperc) {
	# 	cat("\nCell percentages:\n")
	# 	print(prop.table(result$table), digits = 2)  	# cell percentages
	# }
	# if(input$ct_rowperc) {
	# 	cat("\nRow percentages:\n")
	# 	print(prop.table(result$table, 1), digits = 2) # row percentages 
	# }
	# if(input$ct_colperc) {
	# 	cat("\nColumn percentages:\n")
	# 	print(prop.table(result$table, 2), digits = 2) # column percentages
	# }

	print(result$cst, digits = 2)
	# cat(paste("\n",sprintf("%.1f",100 * (sum(result$cst$expected < 5) / length(result$cst$expected))),"% of cells have expected values below 5\n\n"), sep = "")
	cat(paste(sprintf("%.1f",100 * (sum(result$cst$expected < 5) / length(result$cst$expected))),"% of cells have expected values below 5\n\n"), sep = "")
}

plot.crosstab <- function(result) {

	dat <- getdata()[,c(input$ct_var1,input$ct_var2)]
	plots <- list()
	v1 <- input$ct_var1
	v2 <- input$ct_var2

	meltTable <- function(tab) {
		tab <- data.frame(tab)
		lab <- data.frame(rownames(tab))
		names(lab) <- "rnames"
		melt(cbind(lab,tab))
	}

	if(input$ct_std_residuals) {

		tab <- meltTable(result$cst$residuals)
		colnames(tab)[c(2,3)] <- c(input$ct_var1, input$ct_var2)
		plots[['residuals']] <- ggplot(tab, aes_string(x = input$ct_var1, y = "value", fill = input$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .3) +
     					geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
     					geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
     					geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
         			labs(list(title = paste("Deviation (standardized) for ",input$ct_var2," versus ",input$ct_var1, sep = ""), x = input$ct_var1))
	}

	if(input$ct_deviation) {

		tab <- meltTable(result$cst$deviation)
		colnames(tab)[c(2,3)] <- c(input$ct_var1, input$ct_var2)
		plots[['deviation']] <- ggplot(tab, aes_string(x = input$ct_var1, y = "value", fill = input$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .3) + ylim(-1,1) +
         			labs(list(title = paste("Deviation (percentage) for ",input$ct_var2," versus ",input$ct_var1, sep = ""), x = input$ct_var1))
	}

	if(input$ct_expected) {

		tab <- meltTable(result$cst$expected)
		tab$rnames <- factor(tab$rnames,levels=levels(dat[,1]))

		plots[['expected']] <- ggplot(tab, aes_string(x = 'rnames', y = "value", fill = "variable")) +
         			geom_bar(stat="identity", position = "dodge", alpha = .3) +
         			labs(list(title = paste("Expected values for ",input$ct_var2," versus ",input$ct_var1, sep = ""), 
		 					x = ''))
	}

	plots[['observed']] <- ggplot(dat, aes_string(x = input$ct_var1, fill = input$ct_var2)) + geom_histogram(position = "dodge", alpha=.3) +
		labs(list(title = paste("Crosstab of ",input$ct_var2," versus ",input$ct_var1, sep = ""), 
				x = '', y = "Count", fill = input$ct_var2))

	print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

crosstab <- reactive({


	ret_text <- "This analysis requires variables of type factor. Please select another dataset."
 	if(is.null(input$ct_var1) || is.null(input$ct_var2)) return(ret_text)
	if(is.null(inChecker(c(input$ct_var1, input$ct_var2)))) return(ret_text)

  var1 <- input$ct_var1
  var2 <- input$ct_var2

  dat <- getdata()[,c(var1,var2)]

	dnn = c(paste("Group(",input$ct_var1,")",sep = ""), paste("Variable(",input$ct_var2,")",sep = ""))
	tab <- table(dat[,input$ct_var1], dat[,input$ct_var2], dnn = dnn)
	cst <- chisq.test(tab, correct = FALSE)

	# adding the % deviation table
	o <- cst$observed
	e <- cst$expected
	cst$deviation <- (o-e) / e

	nr.plot <- 1 + sum(c(input$ct_expected,input$ct_deviation, input$ct_std_residuals))

	list('cst' = cst, 'table' = tab, plotHeight = 400 * nr.plot)
})

