#view metabolite mapping to KEGG pathways
#has big dependency list, but may  not need most for simple metab olite mapping
#for now limit loading of libraries to only if this tab is selected
#later for server deploy need to install all of these up front

observe({
	if(!input$tool=="keggpath") return()
	if(all(c(require(pathview),require(KEGGREST)))) return() # already loaded 
	check.get.packages(c("KEGGREST","pathview"))
})

#--------------------------------------
#UI objects
#--------------------------------------
#get kegg code and organism name
get.kegg.organism.info<-function(){
	if(!exists("korg")) data(korg)
	return(list(name=korg[,"scientific.name"],code=korg[,"kegg.code"]))
}

#get kegg pathway for organism
# store intermediate results to avoid too many REST calls
get.kegg.pathways<-function(id,DB=NULL){
	if(is.null(DB)) DB<-list()
	#get from KEGG if no entry
	if(is.null(DB$id)){
		DB[[id]]<-keggList("pathway" ,id)
	} 
	#format pathway info for return
	info<-data.frame(name=DB[[id]],code=names(DB[[id]]))
	return(list(pathways=info,DB=DB))
}

#find organism id from name input
output$keggpath_organism_select<-renderUI({
	list(
		selectInput(inputId = "keggpath_organism_name", label = "Name", choices = get.kegg.organism.info()$name, multiple = FALSE),
		selectInput(inputId = "keggpath_organism_code", label = "ID", choices = get.kegg.organism.info()$code, multiple = FALSE)
	)
	
	
})

#kegg pathways for organism
output$keggpath_pathway_select<-renderUI({
	if(is.null(input$keggpath_organism_code)) return()
		# print("---------ENTER----4-----")
		isolate({
			DB<-values$keggpath$pathway_DB
			pathways<-get.kegg.pathways(id=input$keggpath_organism_code,DB=DB)
			p.names<-fixlc(pathways$pathway$name)
			p.codes<-fixlc(pathways$pathway$code)
			values$keggpath$pathway_DB<-pathways$DB # update DB
			values$keggpath$selected_organism_pathway<-list(name=p.names, code=p.codes) # update selections
			list(
				selectInput(inputId = "keggpath_organism_pathway_name", label = "Name", choices = p.names, multiple = FALSE),
				selectInput(inputId = "keggpath_organism_pathway_code", label = "Code", choices = p.codes, multiple = FALSE)
			)	
		})	
})


#data UI to identify fold change and KEGG codes for metabolites
#find organism id from name input
output$keggpath_data_select<-renderUI({
	
	#choose from the data
	vars <- varnames()
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class() | "logical" == getdata_class()
	FC <- vars[isNum]
	keggID<-vars[!isNum]
	
	list(
		selectInput(inputId = "keggpath_data_ID", label = "ID", choices = keggID, multiple = FALSE),
		selectInput(inputId = "keggpath_data_FC", label = "Fold Change", choices = FC, multiple = TRUE),
		checkboxInput(inputId="keggpath_data_log", label="Logarithm", value = FALSE)
	)
	
	
})

#MAIN INPUT UI
ui_keggpath <- function() {
	list(
		wellPanel(
			actionButton("keggpath_calculate", tags$div(icon("fa fa-magic"),tags$label(style="font-size: 30px;","Calculate"),style="font-size: 30px; color: #EF3732;")),	
			h3('Metabolites'),
			tags$details(open="open",tags$summary("Choose"),
				uiOutput("keggpath_data_select")
			),
			h3('Organism'),
			tags$details(open="open",tags$summary("Choose"),
				uiOutput("keggpath_organism_select")
			),
			h3('Pathway'),
			tags$details(open="open",tags$summary("Choose"),	
				uiOutput("keggpath_pathway_select")
			)
		),
	helpModal('Devium','workinprogress',includeHTML("tools/help/workinprogress.html"))		
	)
}	


#MAIN OUTPUT UI
ui_keggpath_tabs<-function(){
	return(
		tabsetPanel(id = "analysistabs",
			tabPanel("Summary", verbatimTextOutput("summary")), # HTML(fancyTableOutput()),
			tabPanel("Plots", uiOutput("keggpath_plot")) # imageOutput("keggpath_plot")
		)	
	)		
}

#--------------------------------------
#--------------------------------------



#update code if name changes
observe({
	if(!is.null(input$keggpath_organism_name)){
		# print("---------ENTER----5-----")
		id<-grep(input$keggpath_organism_name,get.kegg.organism.info()$name)
		updateSelectInput(session, inputId = "keggpath_organism_code", label = "ID",choices = get.kegg.organism.info()$code, selected =get.kegg.organism.info()$code[id])
		#update possible pathways
		isolate({
			DB<-values$keggpath$pathway_DB
			pathways<-get.kegg.pathways(id=input$keggpath_organism_code,DB=DB)
			values$keggpath$pathway_DB<-pathways$DB # update DB
			p.names<-fixlc(pathways$pathway$name)
			p.codes<-fixlc(pathways$pathway$code)
			values$keggpath$selected_organism_pathway<-list(name=p.names, code=p.codes) # update selections
		})	
	}
	#change pathways based on organism code
	if(!is.null(input$keggpath_organism_code)){
		# print("---------ENTER----3-----")
		isolate({
			pathways<-values$keggpath$selected_organism_pathway
			p.names<-fixlc(pathways$pathway$name)
			p.codes<-fixlc(pathways$pathway$code)
			updateSelectInput(session, inputId = "keggpath_organism_pathway_name", label = "Name",choices = p.names)
		})	
	}
	#update pathway code based on chosen name
	if(!is.null(input$keggpath_organism_pathway_name)){
		# print("---------ENTER----2-----")
		isolate({
			obj<-fixlc(values$keggpath$selected_organism_pathway$name)
			id<-(1:length(obj))[obj%in%fixlc(input$keggpath_organism_pathway_name)]
			updateSelectInput(session, inputId = "keggpath_organism_pathway_code", label = "Code",choices=values$keggpath$selected_organism_pathway$code, selected =values$keggpath$selected_organism_pathway$code[id])
		})	
	}
})

#main function 
keggpath <- reactive({
	if(is.null(input$keggpath_calculate)|input$keggpath_calculate==0) return()
	

	isolate({	
		if(is.null(input$keggpath_organism_pathway_code)) return()
		#calculate image name
		map<-gsub("path:","",input$keggpath_organism_pathway_code)
		#create image name based on inputs
		suffix<-gsub(" ","_",paste(c(input$datasets,input$keggpath_data_ID,input$keggpath_data_FC,input$keggpath_data_log),collapse="_"))
		values$keggpath$pathway.image.name<-paste(map,suffix,"png",sep=".")
		print(values$keggpath$pathway.image.name)
		#check if image exists, should check creation keys too
		#look in /www/images for images # has to be in www because including as a tag
		o.dir<-getwd()
		work.path<-paste0(o.dir,"/www/images")
		image.exists<-values$keggpath$pathway.image.name%in%list.files(path = work.path)
		#need to keep track of inputs (e.g. columns for KEGG, FC, log) then can b
		if(image.exists) return() #go to plot
		
		# #create pathway visualization
		keggid<-getdata()[,input$keggpath_data_ID]
		FC<-getdata()[,input$keggpath_data_FC,drop=FALSE]
		error<-tryCatch(rownames(FC)<-make.unique(fixlc(keggid)),error=function(e){TRUE})
		if(input$keggpath_data_log) FC<-log(FC)
		
		#define directory for all objects
		# on error return directory
		tryCatch({
			#set dir for saving file
			setwd(work.path)
			values$keggpath$summary<-pathview(gene.data = NULL,  cpd.data = FC, gene.idtype = "KEGG",kegg.dir = work.path,
			pathway.id = map, species = input$keggpath_organism_code, out.suffix = suffix,
			keys.align = "y", kegg.native = T, match.data=T, key.pos = "topright",new.signature=FALSE)
		}, error=function(e){setwd(work.path);return(NULL)}, finally={setwd(o.dir)})
	})
})

#need to update inputs
observe({

	if(is.null(input$keggpath_calculate))return()
	if(input$keggpath_calculate==0) return()
	keggpath()

})

#info about selected options
summary.keggpath <- function(result) {
	if(is.null(values$keggpath$summary)) return("Please calculate first.")
	values$keggpath$summary
}

# plot.keggpath <- function(result) {
	# if(input$analysistabs == 'Summary'){return()}
	# empty.plot("Network goes here.")	
# }	

# output$keggpath_plot <- renderImage({
	# if(is.null(values$keggpath$pathway.image.name)) return()
 
	
	# filename<-"C:/Users/D/Dropbox/Software/DeviumWeb/other/generic_logo.png"
    # # filename <- tryCatch(normalizePath(file.path('./data',values$keggpath$pathway.image.name)),error=function(e){"C:/Users/D/Dropbox/Software/DeviumWeb/other/generic_logo.png"})
 
	# if(is.null(filename)|length(filename)==0) print(filename);return() 
 
    # # Return a list containing the filename
    # list(src = filename)
  # }, deleteFile = FALSE)

  output$keggpath_plot = renderUI({
	if(is.null(input$keggpath_calculate)) return() # set dependence to trigger image update
	if(input$keggpath_calculate==0) return()
    #check to see if image exists
	image<-values$keggpath$pathway.image.name#get.image()#values$keggpath$pathway.image.name
	src =  paste0("images/",image)
	if(is.null(src)) return() # need default empty image
    tags$img(src=src)
  })

# get.image<-reactive({
	# if(is.null(input$keggpath_calculate)) return() # set dependence to trigger image update
	# if(input$keggpath_calculate==0) return()
	# return(values$keggpath$pathway.image.name)
  # })
