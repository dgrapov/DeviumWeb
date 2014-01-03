ui_About <- function() {

}

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

