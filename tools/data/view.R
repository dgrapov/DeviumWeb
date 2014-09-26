output$view_order <- renderUI({
  if(is.null(input$columns)) return()
  selectInput(inputId = "view_order", label = "Order by:", choices = c("None",input$columns), selected = "None", multiple = FALSE)
})

ui_View <- function() {
  list(wellPanel(
      uiOutput("columns"), 
     	# uiOutput("view_order"), checkboxInput("view_order_desc", "DESC", value = FALSE),
      returnTextInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", '')
    ),
    helpModal('View','view',includeMarkdown("tools/help/view.md"))
  )
}

# output$dataviewer <- renderTable({
# output$dataviewer <- reactive({
output$dataviewer <-renderDataTable({

  if(is.null(input$datasets) || is.null(input$columns)) return()

  # dat <- getdata()
  dat <- cbind(rownames(getdata()),date2character())

  if(!all(input$columns %in% colnames(dat))) return()

  if(input$dv_select != '') {
    selcom <- input$dv_select
    selcom <- gsub(" ", "", selcom)

    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)

    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

  dat <- data.frame(rownames=dat[,1],dat[, input$columns, drop = FALSE])
  dat

  # html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  # html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  # html

}, options = list(bSortClasses = TRUE, aLengthMenu = c(10, 20, 30, 50), iDisplayLength = 10))
