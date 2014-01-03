require(knitr)

options(rstudio.markdownToHTML =
  function(inputFile, outputFile) {     
    require(markdown)
    markdownToHTML(inputFile, outputFile, options = c(""), stylesheet='mymd.css')  
  }
)

helpfiles <- list.files(".", pattern = "*.Rmd")

for(hf in helpfiles) {
	knit2html(hf, options = "", stylesheet = "mymd.css")
}
