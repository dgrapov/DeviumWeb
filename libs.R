
#Radyant libs
libs <- c("shiny", "knitr", "Hmisc", "car", "tools", "gridExtra", "markdown", "R.utils", "psych", "rela", 
          "arm", "plyr", "reshape2", "vegan", "ggplot2", "lubridate", "pander","wordcloud")

#Devium Libs
# libs2 <- c("RJSONIO","pwr","shiny", "car", "AER", "Ecdat", "foreign", "tools", "ggplot2", 
	# "gridExtra", "reshape2", "plyr", "markdown", "R.utils", "psych", "rela", "arm", "xts","fdrtool","pls",
	# "qvalue","pcaMethods","shiny") #having problems with R_fromJSON" not resolved from current namespace (RJSONIO)

	
# libs<-unique(c(libs,libs2))	
		
#function to load from bioconductor
#function to check for packages and attempt to download if not found
check.get.packages<-function(pkg)
	{
		options(warn=-1)
		
		# #make sure bio conductor is one of the repositories
		# #will need a mechanism to make sure this stays upto date
		# if(!all(names(getOption("repos"))%in%"BioCsoft"))
			# {
				# r<-getOption("repos")
				# r["BioCsoft"]<-"http://www.bioconductor.org/packages/2.11/bioc" # needs to be specific version
				# options(repos = r)
				
				# #add layer to call 
				 # #source("http://bioconductor.org/biocLite.R")
				 # #biocLite("package to install")
				 
				 # if install fails
				# #r.version<-paste(sessionInfo()$R.version$major,sessionInfo()$R.version$minor, sep=".")
				# #bioc.url<-paste("http://www.bioconductor.org/packages/",r.version,"/bioc", sep="")
				# #r["BioCsoft"]<-bioc.url # needs to be specific to R version
				# #options(repos = r)
			# }	
		
		res<-character()
		
		need<-as.matrix(sapply(1:length(pkg),function(i)
			{
				
				if(require(pkg[i],character.only = TRUE)==FALSE)
					{
					 res<-c(res,pkg[i])
					}
			}))
			
			need<-as.character(unlist(need[!is.null(need)]))
			if(length(need)>0)
				{
					
					x<-sapply(need,install.packages)
					lib.fun<-function(need){
							sapply(1:length(need), function(i){
							out<-tryCatch(library(need[i], character.only= TRUE), error=function(e){need[i]})
							if(all(out==need[i])){need[i]}
							})
						}
						
					out<-as.character(unlist(lib.fun(need)))
					
					#try bioconductor
					if(length(out)>0){
					cat(paste("Package not found, trying Bioconductor..."),"\n")
					source("http://bioconductor.org/biocLite.R")
					lib.fun.bioc<-function(need){
							sapply(1:length(need), function(i){
							tryCatch( biocLite(need[i]), 
								error=function(e){need[i]})
							})
						}
						
					tmp<-lib.fun.bioc(out)
					final<-lib.fun(tmp)
					if(length(final)>0){cat(paste("could not find package: ",paste(as.character(unlist(final)),collapse=", "),sep=""),"\n")}
					}
					
				}

	}
		