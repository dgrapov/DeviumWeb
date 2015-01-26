ui_About <- function() {
#put version info here

}

# output$about <- renderUI({
  # tags$iframe(
  # seamless="seamless", style="width:100%; height:700px;",
  # src="")
# })

# not yet implemented
update_radyant <- reactive({

	if(!is.null(input$update)) {
		isolate({
			source('update.R')
			# source('ui.R')
			# source('server.R')
			# source('global.R')
		})
	}

})

