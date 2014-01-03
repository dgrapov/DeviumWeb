###############################
# sampling and assignment
###############################

output$rnd_var <- renderUI({
  vars <- varnames()
	isChar <- "character" == getdata_class()
 	vars <- vars[isChar]
  if(length(vars) == 0) return()
  selectInput(inputId = "rnd_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

output$rnd_block <- renderUI({
  vars <- varnames()
	isFct <- "factor" == getdata_class()
 	vars <- vars[isFct]
  if(length(vars) == 0) return()
  selectInput(inputId = "rnd_block", label = "Block variable (select one):", choices = c("None",vars), selected = NULL, multiple = FALSE)
})


ui_random <- function() {
  list(wellPanel(
 	 	uiOutput("rnd_var"),
	  radioButtons(inputId = "rnd_sample", label = "", c("Sample" = "sample", "Assign" = "assign"), selected = "Sample"),
	  conditionalPanel(condition = "input.rnd_sample == 'sample'",
	   numericInput("rnd_sample_size", "Sample size:", min = 1, value = 1)
  	),
	  conditionalPanel(condition = "input.rnd_sample != 'sample'",
	  	numericInput("rnd_nrCond", "Number of conditions:", min = 2, value = 2),
	 	 	uiOutput("rnd_block"),
	    actionButton("rnd_save_treatment", "Save treatment")
  	)
  	),
	 	helpModal('Random','random',includeHTML("tools/help/random.html"))
 	)
}

summary.random <- function(result) {
	if(input$rnd_sample == 'sample') {
		cat("Selected units:\n")
		print(result$sample)
		cat("\nAll units:\n")
		print(result$dat)
		} else {
			cat("Assigned:\n")
			print(result)
		}
}

plot.random <- function(result) {
	"Relevant output is in the Summary tab."
}

random <- reactive({

	ret_text <- "This analysis requires a variable of type character. Enteries should be unique (i.e., no duplicates). Please select another dataset."
	if(is.null(input$rnd_var)) return(ret_text)
	if(is.null(inChecker(c(input$rnd_var)))) return(ret_text)
	if(is.na(input$rnd_sample_size)) return("Please select a sample size of 1 or greater.")

	# example list of names obtained from http://listofrandomnames.com
	dat <- getdata()

	if(input$rnd_sample == 'sample') {

		selDat <- dat[sample(1:nrow(dat), input$rnd_sample_size),, drop = FALSE]
		return(list(sample = selDat, dat = dat))
	} else {

		dat$treatment <- 0

		# <<- needed else ddply doesn't know where to 'look'
		nrCond <<- 1:input$rnd_nrCond

		if(!is.null(input$rnd_block) && input$rnd_block != "None") {

			# dat <- rndnames
			# dat$treatment <- 0
			# input <- list()
			# input$rnd_var <- 'Names'
			# input$rnd_block <- 'Gender'
			# nrCond <- 1:5

			# adapted from http://stackoverflow.com/questions/5399773/how-to-generate-a-random-treatment-variable-by-factor
			 dat <- ddply(dat, c(input$rnd_block), transform, 
			 		treatment = replace(treatment, sample(seq_along(treatment)), nrCond))

		} else {

			dat$treatment <- replace(dat$treatment, sample(seq_along(dat$treatment)), nrCond)
		}

		nrCond <<- NULL
		dat
	}
})

observe({
	if(is.null(input$rnd_save_treatment) || input$rnd_save_treatment == 0) return()
	isolate({
		if(is.character(random())) return()
		changedata(data.frame(as.factor(random()$treatment)), "treatment")
	})
})

ui_sampleSize <- function() {
  list(wellPanel(
	  radioButtons(inputId = "rnd_mean", label = "", c("Mean" = "mean", "Proportion" = "proportion"), selected = "Mean"),
	  conditionalPanel(condition = "input.rnd_mean == 'mean'",
	    numericInput("rnd_mean_err", "Acceptable Error (units, e.g., $10):", min = 0, value = .2, step = .1),
	    numericInput("rnd_mean_s", "Sample std. deviation:", min = 0, value = 3, step = .1)
  	),
	  conditionalPanel(condition = "input.rnd_mean != 'mean'",
	  	numericInput("rnd_prop_err", "Acceptable Error (e.g., .03):", min = 0, value = .1, step = .01),
	    numericInput("rnd_prop_p", "Sample proportion:", min = 0, value = .5, step = .1)
  	),
    numericInput("rnd_z", "Confidence level (z-value):", min = 0, value = 1.96, step = .1),
    numericInput("rnd_incidence", "Incidence rate:", min = 0, value = 1, step = .05),
    numericInput("rnd_response", "Response rate:", min = 0, value = 1, step = .05),
	  radioButtons(inputId = "rnd_pop_correction", label = "Correct for population size:", c("Yes" = "yes", "No" = "no"), selected = "No"),
	  conditionalPanel(condition = "input.rnd_pop_correction == 'yes'",
	    numericInput("rnd_pop_size", "Population size:", min = 1, value = 1000000, step = 1000)
    )),
	 	helpModal('Sample size','sampleSize',includeHTML("tools/help/sampleSize.html"))
 	)
}

summary.sampleSize <- function(result) {
	cat("Required sample size:", result)
	cat("\nRequired contact attempts:", result / input$rnd_incidence / input$rnd_response)

	cat("\n\nChoose a Z-value as follows:\n")
	# cat("90% / 1.64\n95% / 1.96\n99% / 2.58\n99.9% / 3.29")
	cat("90%\t1.64\n95%\t1.96\n99%\t2.58\n99.9%\t3.29")

}

plot.sampleSize <- function(result) {
	"Relevant output is in the Summary tab."
}

sampleSize <- reactive({

	if(is.null(input$rnd_mean)) return("")

	if(input$rnd_mean == 'mean') {

		if(is.na(input$rnd_mean_err)) return("Please select an error value greater 0.")

		n <- (input$rnd_z^2 * input$rnd_mean_s^2) / input$rnd_mean_err^2

		# if(!is.null(input$rnd_pop_size)) n <- n * input$rnd_pop_size / ((n - 1) + input$rnd_pop_size)
		if(input$rnd_pop_correction == 'yes') n <- n * input$rnd_pop_size / ((n - 1) + input$rnd_pop_size)

		return(ceiling(n))

	} else {

		if(is.na(input$rnd_prop_err)) return("Please select an error value greater 0.")

		n <- (input$rnd_z^2 * input$rnd_prop_p * (1 - input$rnd_prop_p)) / input$rnd_prop_err^2

		# if(!is.null(input$rnd_pop_size)) n <- n * input$rnd_pop_size / ((n - 1) + input$rnd_pop_size)
		if(input$rnd_pop_correction == 'yes') n <- n * input$rnd_pop_size / ((n - 1) + input$rnd_pop_size)

		return(ceiling(n))
	}
})
