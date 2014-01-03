shinyUI(
  pageWithSidebar(


    # Using a navbar rather than headerPanel to display app title
    headerPanel(''),
    sidebarPanel(

      # tags$head(tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript') # ),
      includeCSS('www/style.css'),
      tags$head(tags$script(src = "js/jquery-ui.min.js")),
      # tags$head(tags$script(src = "js/lr.js")),
      # includeHTML("www/js/sel2.js"),
      # includeHTML('www/js/lr.js'), 
      getTool("tool"),

      wellPanel(
        uiOutput("datasets")
      ),

      # find the appropriate UI
      uiOutput("ui_finder")
    ),

    mainPanel(
      conditionalPanel(condition = "input.datasets != ''",
        conditionalPanel(condition = "input.tool == 'data'", 
          uiOutput("ui_data_tabs")
        ),
        conditionalPanel(condition = "input.tool != 'data'",
          uiOutput("ui_analysis_tabs")
        )
      )
    )
  )
)

# as.Date('2000-01-01')