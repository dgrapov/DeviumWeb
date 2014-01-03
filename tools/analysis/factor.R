# variable selection - factor analysis
output$preFactor_vars <- renderUI({

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectInput(inputId = "preFactor_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_preFactor <- function() {
  list(wellPanel(
  	uiOutput("preFactor_vars")),
		helpModal('Pre-factor analysis','preFactor',includeHTML("tools/help/preFactor.html"))
	)
}


summary.preFactor <- function(result) {

	btest <- result$btest
	prefac <- result$prefac

	cat("\n\nPre-factor analysis diagnostics:\n\n")
	cat("Bartlett test of sphericity\n")
	cat("Chi-square: ", round(btest$chisq,3), "\n")
	cat("Degrees of freedom: ", btest$df, "\n")
	cat("p-value: ", round(btest$p.value,3), "\n")
	cat("H0: Correlation Matrix = Identity Matrix, i.e., variables are not correlated\n")

	cat("\nKaiser-Meyer-Olkin measure of sampling adequacy\nKMO: ", round(prefac$KMO,3), "\n")
	cat("\nMeasures of sampling adequacy:\n")
	print(prefac$MSA, digits = 3)
	cat("\n")

	ev <- prefac$Eigenvalues[,'0']
	ev.var <- ev/sum(ev)
	ev.cvar <- cumsum(ev.var)
	df <- data.frame(1:length(ev),ev,ev.var,ev.cvar)
	colnames(df) <- c("Factor","Eigen Values","% of variance","Cumulative %") 
	# print(df, digits = 3, row.names = FALSE)
	df
}

plot.preFactor <- function(result) {

	prefac <- result$prefac
	ev <- prefac$Eigenvalues[,'0']
	plot(ev, type = 'b', col = 'blue', main = "Screeplot of Eigenvalues", ylab = "Eigenvalues", xlab = "# of factors")
	abline(1,0, col = 'red')
}

preFactor <- reactive({

	if(is.null(input$preFactor_vars) || length(input$preFactor_vars) < 2) return("Please select two or more variables")

	ret_text <- "This analysis requires a multiple variables of type numeric or integer. Please select another dataset."
	if(is.null(inChecker(c(input$preFactor_vars)))) return(ret_text)

	dat <- getdata()[,input$preFactor_vars]
	if(nrow(dat) < ncol(dat)) return("Data has more variables than observations. Please reduce the number of variables.")

	btest <- cortest.bartlett(cor(dat), nrow(dat))

	# require(psych)
	# require(rela)
	# dat <- microvan[,3:32]
	# head(dat)
	prefac <- local_paf(as.matrix(dat))
	# prefac <- paf(as.matrix(dat))

	return(list(btest = btest, prefac = prefac))
})

# variable selection - factor analysis
output$factor_vars <- renderUI({

  varCls <- getdata_class()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  # No memory when you want it :)
  # sel <- NULL
  # if(!is.null(input$preFactor_vars) || !is.null(inChecker(c(input$preFactor_vars)))) sel <- input$preFactor_vars

  selectInput(inputId = "factor_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

fac_method <- c('Principal components' = 'PCA', 'Maximum Likelihood' = "maxlik")
fac_rotation <- c('Varimax' = 'varimax', 'None' = 'none')

ui_fullFactor <- function() {
  list(wellPanel(
    uiOutput("factor_vars"), 
    selectInput("fac_method", label = "Method:", choices = fac_method, selected = fac_method[1], multiple = FALSE),
    numericInput("fac_number", label = "Number of factors:", min = 1, value = 1),

    conditionalPanel(condition = "input.analysistabs != 'Plots'",
	    HTML("<label>Format loadings:</label>"),
			div(class="row-fluid",
	    	div(class="span6", numericInput("fac_cutoff", label = "", min = 0, max = 1, value = 0, step = .05) ),
	      # div(class="span6", checkboxInput("fac_condFormat", "Conditional", value = FALSE) )
	      div(class="span6", checkboxInput("fac_sort", "Sort", value = FALSE) )
	    )
	  ),

    radioButtons("fac_rotation", label = "Rotation:", fac_rotation, selected = 'Varimax'),
    actionButton("fac_savescores", "Save scores")
  	),
		helpModal('Factor analysis','fullFactor',includeHTML("tools/help/fullFactor.html"))
	)
}

summary.fullFactor <- function(result) {

	cat("Factor loadings matrix:\n")

	ifelse(input$fac_sort, loadings <- fa.sort(result$loadings), loadings <- result$loadings)

	print(loadings, cutoff = input$fac_cutoff, digits = 3)

	# data(Bechtoldt.1)
	# sorted <- mat.sort(Bechtoldt.1,fa(Bechtoldt.1,5))
	# sorted
	# cor.plot(sorted)

	# ts <- fa(item.sim(16),2,rotate="oblimin")
	# fa.sort(ts$loadings)
	# fa.sort(test.simple)

	communalities <- data.frame(1 - result$uniqueness)
	colnames(communalities) <- ""
	cat("\nAttribute communalities:\n")
	print(communalities, digits = 3)

	cat("\nFactor scores (max 30 shown):\n")
	scores <- as.data.frame(result$scores)
	print(scores[1:min(nrow(scores),30),, drop = FALSE], digits = 3)

}

fullFactor_fancy_tab <- function() {

	if(input$fac_condFormat) {

		res <- fullFactor()
		if(!is.null(res)) {

			res <- round(res$loading[],3)
			abs_res <- abs(res)
			row_max <- apply(abs_res, 1, max)

			ind_max <- which(abs_res == row_max , arr.ind = TRUE)
			ind_not_max <- which(abs_res < row_max, arr.ind = TRUE)
			emphasize.strong.cells(ind_max)
			emphasize.cells(ind_not_max)
			pander.return(res, style='rmarkdown', split.tables=Inf)
		} else {
			NULL
		}
	} else {
		NULL
	}
}

plot.fullFactor <- function(result) {

	if(result$factors < 2) return()

	df <- round(as.data.frame(result$loadings[]),3)
	rnames <- rownames(df)
	cnames <- colnames(df)
	plots <- list()
	pnr <- 1
	ab_df <- data.frame(a=c(0,0), b=c(1, 0))

	for(i in 1:(length(cnames)-1)) {
		for(j in (i+1):length(cnames)) {

			i_name <- cnames[i]
			j_name <- cnames[j]

		  df2 <- cbind(df[, c(i_name,j_name)],rnames)
  		plots[[pnr]] <- ggplot(df2, aes_string(x = i_name, y = j_name, color = 'rnames', label = 'rnames')) + geom_text() + theme(legend.position = "none") +
  			xlim(-1,1) + ylim(-1,1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
  		pnr <- pnr + 1
  	}
	}

	print(do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots))))))
}

fullFactor <- reactive({

	if(is.null(input$factor_vars) || length(input$factor_vars) < 2) return("Please select two or more variables")
	if(is.null(input$fac_number)) return("Number of factors should be > 1.")

	ret_text <- "This analysis requires a multiple variables of type numeric or integer. Please select another dataset."
	if(is.null(inChecker(c(input$factor_vars)))) return(ret_text)

	dat <- getdata()[,input$factor_vars]
	if(nrow(dat) < ncol(dat)) return("Data has more variables than observations. Please reduce the number of variables.")

	nrFac <- max(1,as.numeric(input$fac_number), na.rm = TRUE)
	if(nrFac > ncol(dat)) {
		cat("The number of factors cannot exceed the number of variables.\n")
		nrFac <- ncol(dat)
	}

	if(input$fac_method == 'PCA') {
		fres <- principal(dat, nfactors=nrFac, rotate=input$fac_rotation, scores=TRUE, oblique.scores=FALSE)
	} else {
		fres <- factanal(dat, nrFac, rotation=input$fac_rotation, scores='regression')
		fres$rotation <- input$fac_rotation
	}

	# nr.plots <- factorial(c(nrFac,2))
	# fres$plotHeight <- 650 * (nr.plots[1] / nr.plots[2])
	# nr.plots <- (nrFac * (nrFac - 1)) / 2
	nr.plots <- (nrFac * (nrFac - 1)) / 2
	# fres$plotHeight <- 400 * nr.plots
	ifelse(nr.plots > 2, fres$plotHeight <- 350 * ceiling(nr.plots/2), fres$plotHeight <- 350)
	ifelse(nr.plots > 1, fres$plotWidth <- 700, fres$plotWidth <- 350)
	fres
})

# save factor scores when action button is pressed
observe({
	if(is.null(input$fac_savescores) || input$fac_savescores == 0) return()
	isolate({
		if(is.character(fullFactor())) return()
		facscores <- data.frame(fullFactor()$scores)
		changedata(facscores, paste0("fac",1:input$fac_number))
	})
})

# local 'copy' of the paf function from the rela package
# the line below causes an error if any covariance is equal to 0 which
# is not what is intended
# yy <- sort(abs(as.numeric(var(object))))

local_paf <- function (object, eigcrit = 1, convcrit = 0.001) {
    if (is.null(row.names(t(object)))) {
        print("Dataset must be coerced into matrix from prior data frame.")
    }
    else {
        if (!is.numeric(object)) {
            print("Your dataset is not a numeric object.")
        }
        else {
            object <- (na.omit(object))
            # yy <- sort(abs(as.numeric(var(object))))
            yy <- sort(abs(as.numeric(diag(var(object)))))
            yy <- yy[1]
            yy <- ifelse(yy == 0, NA, yy)
            if (is.na(yy)) {
                print("One of your variables is a constant. Constants are disallowed as part of a scale.")
            }
            else {
                if (dim(cbind(object))[2] < 2) {
                  print("Your set contains only one variable. At least two are required.")
                }
                else {
                  if (dim(cbind(object))[1] < 2) {
                    print("Your set contains only one case. You need at least two cases.")
                  }
                  else {
                    same <- function(x) {
                      for (i in 1:ncol(x)) {
                        for (j in 1:ncol(x)) {
                          if (i != j) {
                            test <- x[, i] - x[, j]
                            if (sum(abs(test)) == 0) {
                              print(paste("WARNING: Items ", 
                                i, " (", colnames(x)[i], ") ", 
                                "and ", j, " (", colnames(x)[j], 
                                ") ", "are duplicates."))
                            }
                          }
                        }
                      }
                    }
                    same(object)
                    x <- na.omit(object)
                    x <- cor(x)
                    N <- nrow(na.omit(object))
                    options(digits = 5)
                    x <- as.matrix(x)
                    x1 <- x
                    rownames(x1) = colnames(x)
                    bt <- -((N - 1) - ((2 * ncol(x) + 5)/6)) * 
                      log(det(x))
                    invx <- solve(x)
                    ssqr <- matrix(0, nrow(x), ncol(x))
                    diag(ssqr) <- diag(invx)
                    ssqr <- solve(ssqr)
                    Q <- ssqr %*% invx %*% ssqr
                    colnames(Q) <- colnames(x)
                    rownames(Q) <- colnames(x)
                    qdi <- diag(diag(Q))
                    sqdi <- solve(qdi)
                    ssqdi <- sqrt(sqdi)
                    Qr <- ssqdi %*% Q %*% ssqdi
                    colnames(Qr) <- colnames(x)
                    rownames(Qr) <- colnames(x)
                    xnod <- x
                    diag(xnod) <- 0
                    Qrnod <- Qr
                    diag(Qrnod) <- 0
                    KMO <- sum(xnod^2)/(sum(xnod^2) + sum(Qrnod^2))
                    MSA <- 0
                    for (i in 1:nrow(xnod)) (MSA <- c(MSA, sum(xnod[i, 
                      ]^2)/(sum(xnod[i, ]^2) + sum(Qrnod[i, ]^2))))
                    MSA <- matrix(MSA[-1], , 1)
                    rownames(MSA) <- colnames(x)
                    colnames(MSA) <- "MSA"
                    comm0 <- 1 - diag(Q)
                    comm1 <- comm0
                    allcomm <- 0
                    diffscores <- 0
                    iter <- 0
                    count <- 0
                    eigenval <- 0
                    x0 <- x
                    repeat {
                      allcomm <- cbind(allcomm, comm1)
                      eigs <- 0
                      diag(x) <- comm1
                      for (i in 1:length(eigen(x)$values)) if (eigen(x0)$values[i] > 
                        eigcrit) {
                        eigs <- c(eigs, eigen(x)$values[i])
                      }
                      eigs <- eigs[-1]
                      eigenval <- cbind(eigenval, eigen(x)$values)
                      eigmat <- sqrt(diag(eigs, length(eigs), 
                        length(eigs)))
                      eigvecr <- matrix(eigen(x)$vector[, 0:length(eigs)], 
                        , length(eigs))
                      one <- c((1:ncol(eigvecr))/(1:ncol(eigvecr)))
                      factload <- eigvecr %*% eigmat
                      comm2 <- t(rowsum(t(factload^2), one))
                      dif <- abs(comm1 - comm2)
                      iter <- iter + 1
                      count <- c(count, iter)
                      diffscores <- cbind(diffscores, dif)
                      comm1 <- comm2
                      endtest <- matrix(1, nrow(dif), 1)
                      for (i in 1:nrow(dif)) if (dif[i, 1] < 
                        convcrit) {
                        endtest[i, 1] = NA
                      }
                      if (length(na.omit(endtest)) == 0) 
                        break
                    }
                    firstlast <- cbind(comm0, comm1)
                    colnames(firstlast) = c("Initial Communalities", 
                      "Final Extraction")
                    allcomm <- cbind(allcomm, comm1)
                    allcomm <- allcomm[, -1]
                    colnames(allcomm) = count
                    diffscores <- diffscores[, -1]
                    colnames(diffscores) = c(1:iter)
                    eigenval <- cbind(eigen(x0)$values, eigenval[, 
                      -1])
                    colnames(eigenval) = c(0:iter)
                    rownames(factload) <- colnames(x)
                    facttest <- factload[, 1]
                    for (i in 1:length(facttest)) if (facttest[i] < 
                      0) {
                      facttest[i] <- NA
                    }
                    if (length(na.omit(facttest)) == 0) 
                      (factload[, 1] <- -factload[, 1])
                    correp <- factload %*% t(factload)
                    residuals <- x - correp
                    colnames(correp) <- colnames(x)
                    rownames(correp) <- colnames(x)
                    colnames(residuals) <- colnames(x)
                    rownames(residuals) <- colnames(x)
                    diag(correp) <- 1
                    diag(residuals) <- 0
                    RMS <- sqrt(sum(residuals^2)/((nrow(x)^2) - 
                      (nrow(x))))
                    output <- list("PRINCIPAL AXIS FACTORING", 
                      Correlation = x1, Anti.Image.Cov = Q, Anti.Image.Cor = Qr, 
                      KMO = KMO, MSA = MSA, Bartlett = bt, Communalities = firstlast, 
                      Iterations = iter, Eigenvalues = eigenval, 
                      Communality.Iterations = allcomm, Criterion.Differences = diffscores, 
                      Factor.Loadings = factload, Reproduced.Cor = correp, 
                      Residuals = residuals, RMS = RMS)
                    ob <- as.character(match.call()[2])
                    cl <- call("paf", object = ob, eigcrit = eigcrit, 
                      convcrit = convcrit)
                    output$call <- cl
                    output$items <- names(output)
                    class(output) <- c("paf", class(output))
                    output
                  }
                }
            }
        }
    }
}

