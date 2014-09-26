ui_Manage <- function() {
  list(wellPanel(
      # radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".rda"),
      tags$details(tags$summary("Load"),	
		  radioButtons(inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".csv"),
		  conditionalPanel(condition = "input.dataType != 'clipboard' && input.dataType != 'examples'",
			conditionalPanel(condition = "input.dataType == 'csv'",
			  checkboxInput('header', 'Header', TRUE),checkboxInput('csv_rownames', 'Rownames', TRUE),
			  checkboxInput('transpose', 'Transpose', FALSE),
			  radioButtons('sep', '', c(Comma=',', Semicolon=';', Tab='\t'), 'Comma')
			),
			fileInput('uploadfile', '', multiple=TRUE)
		  ),
      conditionalPanel(condition = "input.dataType == 'clipboard'",
        actionButton('loadClipData', 'Paste data')
      ),
      conditionalPanel(condition = "input.dataType == 'examples'",
        actionButton('loadExampleData', 'Load examples')
      )
	 ) 
    ),
    wellPanel(
	  tags$details(tags$summary("Save"),	
      radioButtons(inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".csv"),
      checkboxInput("man_add_descr","Add/edit data description", FALSE),
      conditionalPanel(condition = "input.saveAs == 'clipboard'",
        actionButton('saveClipData', 'Copy data')
      ),
      conditionalPanel(condition = "input.saveAs != 'clipboard'",
        downloadButton('downloadData', 'Save')
      )
	 ) 
    ),
    wellPanel(
	  tags$details(tags$summary("Remove"),	
      uiOutput("removeDataset"),
      actionButton('removeDataButton', 'Remove data')
	 )
	) ,
	 wellPanel(
	  tags$details(tags$summary("Merge"),	
		selectInput(inputId = "MainMergeDataSet", label = "Primary Dataset:", choices = c("------",values$datasetlist), selected = "------", multiple = FALSE),
		selectInput(inputId = "SecondaryMergeDataSet", label = "Merge with:", choices = c("------",values$datasetlist),selected = "------", multiple = FALSE),
		actionButton('mergeDataButton', 'Merge data sets')
	 ) 
    ),
    helpModal('Manage','manage',includeMarkdown("tools/help/manage.md"))
  )
}

observe({
  # 'reading' data to clipboard
  if(is.null(input$loadClipData) || input$loadClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {
      
      dat <- try(read.table("clipboard", header = TRUE, sep = '\t'), silent = TRUE)
      if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
    } else { 

      dat <- try(read.table(pipe("pbpaste"), header = TRUE, sep = '\t'), silent = TRUE)
      if(is(dat, 'try-error')) dat <- c("Data from clipboard was not well formatted. Try exporting the data to csv format.")
    }

    values[['xls_data']] <- as.data.frame(dat)
    values[['datasetlist']] <- unique(c('xls_data',values[['datasetlist']]))
    updateRadioButtons(session = session, inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda")
  })
})

observe({
  # loading all examples files (linked to helpfiles)
  if(is.null(input$loadExampleData) || input$loadExampleData == 0) return()
  isolate({

    # setwd('~/Dropbox/radyant/inst/marketing/')

    path <- "www/examples/"
    examples <- list.files(path)

    # ex_frame1 <- data.frame(examples)
    # ex_frame1$path <- path

    for(ex in examples) {
      ext <- file_ext(ex)
      loadUserData(ex, paste0(path,ex))
    }

    # only available for my students
    path <- "www/MGT475_data/"
    examples <- list.files(path)

    # ex_frame2 <- data.frame(examples)
    # ex_frame2$path <- path

    for(ex in examples) {
      ext <- file_ext(ex)
      loadUserData(ex, paste0(path,ex))
    }

    # updateRadioButtons(session = session, inputId = "dataType", label = "Load data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard", "examples" = "examples"), selected = ".rda")
  })
})

observe({
  # 'saving' data to clipboard
  if(is.null(input$saveClipData) || input$saveClipData == 0) return()
  isolate({
    os_type <- .Platform$OS.type
    if (os_type == 'windows') {
      write.table(getdata(), "clipboard", sep="\t", row.names=FALSE)
    } else { 
      write.table(getdata(), file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    }
    updateRadioButtons(session = session, inputId = "saveAs", label = "Save data:", c(".rda" = "rda", ".csv" = "csv", "clipboard" = "clipboard"), selected = ".rda")
  })
})

# output$downloadData <- downloadHandler(
#   filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
#   content = function(file) {

#     ext <- input$saveAs
#     robj <- input$datasets

#     # only save selected columns
#     # assign(robj, getdata()[,input$columns])
#     assign(robj, getdata())

#     if(ext == 'rda') {
#       save(list = robj, file = file)
#     } else if(ext == 'csv') {
#       write.csv(get(robj), file)
#     }
#   }
# )

observe({
  if(is.null(input$removeDataButton) || input$removeDataButton == 0) return()
  isolate({
    for(rem in input$removeDataset) {
      values[[rem]] <- NULL
    }
    # datasets <<- datasets[-which(datasets == input$datasets)]
    datasets <- values[['datasetlist']]
    if(length(datasets) == length(input$removeDataset)) {
      datasets <- ""
    } else {
      # datasets <- datasets[-which(datasets == input$removeDataset)]
      datasets <- datasets[-which(datasets %in% input$removeDataset)]
    }

    values[['datasetlist']] <- datasets
  })
})

loadUserData <- function(filename, uFile) {

  ext <- file_ext(filename)
  # objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(filename))
  objname <- sub(paste(".",ext,sep = ""),"",basename(filename))
  ext <- tolower(ext)

  if(ext == 'rda' || ext == 'rdata') {
    # objname will hold the name of the object(s) inside the R datafile
    robjname <- load(uFile)

    if(length(robjname) > 1) {

      values[[objname]] <- data.frame(get(robjname[-which(robjname == "description")]))
      values[[paste0(objname,"_descr")]] <- get("description")
      
    } else {

      values[[objname]] <- data.frame(get(robjname))  # only work with data.frames
    }
  }

  if(length(values[['datasetlist']]) == 0 || values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(objname)
  } else {
    values[['datasetlist']] <- unique(c(objname,values[['datasetlist']]))
  }

  if(ext == 'sav') {
    values[[objname]] <- as.data.frame(as.data.set(spss.system.file(uFile)))
  } else if(ext == 'dta') {
    values[[objname]] <- read.dta(uFile)
  } else if(ext == 'csv') {
	if(input$csv_rownames){rownames<-1} else {rownames<-NULL}
	if(input$transpose){
		values[[objname]] <- fixlt(read.csv(uFile, header=input$header, sep=input$sep,row.names=rownames))
	} else {
		values[[objname]] <- read.csv(uFile, header=input$header, sep=input$sep,row.names=rownames)
	}	
  }
}

loadPackData <- function(pFile) {

  robjname <- data(list = pFile)
  dat <- get(robjname)

  if(pFile != robjname) return("R-object not found. Please choose another dataset")

  if(is.null(ncol(dat))) {
    return()
  }

  values[[robjname]] <- dat

  if(values[['datasetlist']][1] == '') {
    values[['datasetlist']] <- c(robjname)
  } else {
    values[['datasetlist']] <- unique(c(robjname,values[['datasetlist']]))
  }
}

output$datasets <- renderUI({

  inFile <- input$uploadfile
 
  # if(!is.null(inFile)) loadUserData(inFile$name, inFile$datapath)
  if(!is.null(inFile)) {
    # iterating through the files to upload
	if(is.null(values$lastUploadedTempFile)){current<-""} else {current<-values$lastUploadedTempFile}
	if(is.null(input$uploadfile$datapath)){toload<-"none"} else {toload<-input$uploadfile$datapath}
	if(!current==toload){
    isolate({
      for(i in 1:(dim(inFile)[1])) {
        loadUserData(inFile[i,'name'], inFile[i,'datapath'])
        # unlink(inFile[i,'datapath'], recursive = FALSE, force = TRUE)
      }
	  values$lastUploadedTempFile<-input$uploadfile$datapath # preventing repeated loading on datsetlist change
    })
	}
	
  }

  # # loading package data
  # if(input$packData != "") {
  #   if(input$packData != lastLoaded) {
  #     loadPackData(input$packData)
  #     lastLoaded <<- input$packData 
  #   }
  # }

  # Drop-down selection of data set
  # selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
  selectInput(inputId = "datasets", label = "Datasets:", choices = values$datasetlist, selected = values$datasetlist[1], multiple = FALSE)
})

output$removeDataset <- renderUI({
  # Drop-down selection of data set
  selectInput(inputId = "removeDataset", label = "Remove data from memory:", choices = values$datasetlist, selected = NULL, multiple = TRUE)
})

output$packData <- renderUI({
  selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

# observe({
#   if(is.null(input$loadExampleData) || input$loadExampleData == 0) return()
#   isolate({
#     values[[paste0(input$datasets,"_descr")]] <- input$man_data_descr
#     updateCheckboxInput(session = session,  "man_add_descr","Add/edit data description", FALSE)
#   })
# })

output$downloadData <- downloadHandler(
  filename = function() { paste(input$datasets,'.',input$saveAs, sep='') },
  content = function(file) {

    ext <- input$saveAs
    robj <- input$datasets

    if(ext == 'rda') {
      if(input$man_data_descr != "") {

        # save data description
        assign(robj, getdata())
        description <- input$man_data_descr
        save(list = c(robj,"description"), file = file)

        # if(values[[paste0(input$datasets,"_descr")]] != input$man_data_descr) {
        #   updateCheckboxInput(session = session,  "man_add_descr","Add/edit data description", FALSE)
        #   values[[paste0(input$datasets,"_descr")]] <- input$man_data_descr
        # }

      } else {

        assign(robj, getdata())
        save(list = robj, file = file)
      }
    } else if(ext == 'csv') {
      assign(robj, getdata())
      write.csv(get(robj), file)
    }
  }
)

output$removeDataset <- renderUI({
  # Drop-down selection of data set
  selectInput(inputId = "removeDataset", label = "Remove data from memory:", choices = values$datasetlist, selected = NULL, multiple = TRUE)
})

output$packData <- renderUI({
  selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

output$htmlDataExample <- reactive({
  if(is.null(input$datasets)) return()

  dat <- getdata()

  # necessary when deleting a dataset
  if(is.null(dat)) return()

  # Show only the first 10 rows
  nr <- min(10,nrow(dat))
  dat <- data.frame(dat[1:nr,, drop = FALSE])

  dat <- date2character_dat(dat)

  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html

})

#give data structure for debug
output$ManageDataStr <- renderPrint({
  if(is.null(input$datasets)) return()

  dat <- getdata()

  # necessary when deleting a dataset
  if(is.null(dat)) return()
  #get simple summary
  isfct<-sapply(dat,is.factor)
  isnum<-sapply(dat,is.numeric)|sapply(dat,is.integer)
  datasum<-list(dimensions = data.frame(rows=dim(dat)[1],columns=dim(dat)[2]),
				factors = tryCatch(summary(dat[,isfct,drop=FALSE]),error=function(e){NULL}),
				zeros = sum(dat[,isnum,drop=FALSE]==0),
				"missing values" = sum(is.na(dat[,isnum,drop=FALSE]))
  )
  print(datasum)
 })  

#merge 2 data sets based on rownames
observe({
	if(is.null(input$MainMergeDataSet)||is.null(input$mergeDataButton)||input$mergeDataButton == 0) return()
	if(input$MainMergeDataSet=="------"|input$SecondaryMergeDataSet=="------") return()
		#merge on row name
		isolate({
			d1<-values[[input$MainMergeDataSet]]
			d2<-values[[input$SecondaryMergeDataSet]]
			merged.data<-merge(data.frame(names=rownames(d1),d1), data.frame(names=rownames(d2),d2), by="names", all=TRUE)
			names<-merged.data[,1]
			rownames(merged.data)<-names
			merged.data<-merged.data[,-1,drop=FALSE]
			name<-paste("merged",input$MainMergeDataSet,input$SecondaryMergeDataSet,sep="_")
			values[[name]]<-merged.data
			values[['datasetlist']] <- c(values$datasetlist,name)
		})
})