#test for normality
normal.test<-function(var,test.method){
			check.get.packages("nortest")
                test<-switch(test.method,
				# add "Kolmogorov-Smirno","Shapiro-Francia"
                "Shapiro-Wilk"  = shapiro.test(var)$p.value,
                "Cramer-von Mises"   = cvm.test(var)$p.value,
                "Anderson-Darling" = tryCatch(ad.test(var)$p.value,error=function(var) 0))        
                if(is.na(test)){test<-0}
                return(test)    
        }

#transform to normal
transform.to.normal<-function(data,data.name="",test.method= "Anderson-Darling", alpha = 0.05 , force.positive=TRUE, transformation="none"){	
		check.get.packages(c("plyr","nortest","car"))
		#function to carry out the transformation
		trans<-function(var,transformation,test.method,alpha,force.positive)
			{
				if(force.positive){var<-var + abs(min(var))}
				p.val<-normal.test(var,test.method)
				if(p.val <= alpha) 
					{
							switch(transformation,
							"log"		= .local<-function(var){ list(data=log(var+1), transformation = transformation)},
							"none"		= .local<-function(var){list(data=var,transformation="none")},
							"BOX-COX" 	= .local<-function(var)
								{
									power<-tryCatch(as.numeric(unlist(powerTransform(na.omit(var))$lambda )),error=function(e){NA}) # for negatives family="yjPower"
									if(is.na(power)) { out<-var }else{ out<-var^power }
									list(data=out,transformation=paste(round(power,3),"power", sep=" "))
								})
							
					} else {
							.local<-function(var){list(data=var,transformation="none")}
							}
							
					obj<-.local(var)	
					return(list(data=as.data.frame(obj$data),p.value = p.val,transformation=obj$transformation))
			}
			
			
		#ignore non-numeric
		data<-data[sapply(1:ncol(data), function(i) {class(data[,i])=="numeric"})]
		
		obj<-colwise(trans) (data, transformation,test.method=test.method,alpha=alpha,force.positive)
		trans.data<-as.data.frame(as.list(obj[1,]));colnames(trans.data)<-colnames(data)
		diagnostics=as.data.frame(t(obj[-1,]));colnames(diagnostics)<-c("p-value","transformation")
		out<-list(trans.data, diagnostics=diagnostics);names(out)<-paste(data.name,c("transformed","transformed.diagnostics"),sep=".")
		return(out)
	}
	
#function to store or return transformed object
transform.to.normal.output<-function(obj,name="transformed.data", envir=devium){
		#object stored: get("devium.data.transformation.results",envir)
		#diagnostics
		tmp<-obj[[2]]
		diagnostics<-as.data.frame(matrix(unlist(tmp),ncol=2));dimnames(diagnostics)<-dimnames(tmp)		# need to break list else gwidget doesn't show properly
		#data
		data<-obj[[1]]
		data.name<-names(obj)
		
		#Determine placement of output for EXCEL
		data.list<-list(data,diagnostics)
		list.names<-matrix(data.name)
		start.row<-1;spacer<-1;start.col<-1
		direction<-"horizontal"

		#assign complete object to envir = vdevium
		assign("devium.data.transformation.results",list(data=data, diagnostics=diagnostics,
		placement=list.placement.full(data.list,list.names,direction="horizontal",start.col,start.row,spacer)),envir=envir)
		
		#add results to global environment
		assign(names(obj)[1],as.data.frame(data),envir=.GlobalEnv)
		assign(names(obj)[2],data.frame(p.value=diagnostics[,1,drop=FALSE],transformation=as.factor(unlist(diagnostics[,2,drop=FALSE]))),envir=.GlobalEnv)
		}

#scaling for rows or columns
scale.data<-function(data, type="median", dim=1,positive.only=TRUE){
	
	#input needs to be numeric
	#make.positive adds minimum to all values to get rid of negatives
	if(type=="sum"){
		val<-apply(data,dim,sum)
		res<-sweep(data,dim,val,"/")
	}
	
	if(type=="l2"){
		val<-apply(data^2,dim,sum)^.5
		res<-sweep(data,dim,val,"/")
	}
	
	if(type%in%c("mean","median")){
		val<-apply(data,dim,type)
		res<-sweep(data,dim,val,"-")
		if(positive.only) res<-make.positive(res)
	}
	
	if(type=="uv"){
		val<-apply(data,dim,sd)
		res<-sweep(data,dim,val,"/")
	}
	
	if(type=="pareto"){
		val<-apply(data,dim,sd)^.5
		res<-sweep(data,dim,val,"/")
	}
	# could add level and vast scaling http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1534033/table/T1/
	if(type=="range_scale"){
		if(dim==2){
			res<-data.frame(do.call("cbind",lapply(1:ncol(data),function(i)
			{
				obj<-data[,i]
				tmp<-range(obj)
				(obj-tmp[1])/(tmp[2]-tmp[1])
			})))
		} else {
			res<-data.frame(do.call("rbind",lapply(1:nrow(data),function(i)
			{
				obj<-data[i,]
				tmp<-range(obj)
				(obj-tmp[1])/(tmp[2]-tmp[1])
			})))

		}
		colnames(res)<-colnames(data)
	}		
	
	return(res)
}
	
#scale data within  a range
#rescaling based on: http://cran.r-project.org/doc/contrib/Lemon-kickstart/rescale.R
rescale<-function(x,newrange) {
	 if(nargs() > 1 && is.numeric(x) && is.numeric(newrange)) {
	  # if newrange has max first, reverse it
	  if(newrange[1] > newrange[2]) {
	   newmin<-newrange[2]
	   newrange[2]<-newrange[1]
	   newrange[1]<-newmin
	  }
	  xrange<-range(x)
	  if(xrange[1] == xrange[2]) stop("can't rescale a constant vector!")
	  mfac<-(newrange[2]-newrange[1])/(xrange[2]-xrange[1])
	  invisible(newrange[1]+(x-xrange[1])*mfac)
	 }
	 else {
	  cat("Usage: rescale(x,newrange)\n")
	  cat("\twhere x is a numeric object and newrange is the min and max of the new range\n")
	 }
}
	
#missing values static imputation 
impute.missing<-function(data, method="min", scalar=1, report=FALSE){
		# data should be a matrix or data frame (samples as rows)
		# methods can be any function
		# scalar will be used to multiply imputed value and can be length one or longer 
		# for example a random normal distribution can be used to generate a scalar to slightly randomize imputed value
		# report = TRUE will return the imputed data in a list and give a report of the number of missing values for each sample and variable as seprate list items
		
		na.id<-is.na(data)
		fill<-apply(data,2,method,na.rm=TRUE)*scalar
		
		#impute
		fixed<-do.call("cbind",lapply(1:ncol(data), function(i)
		{
			#what is being replaced
			obj<-data[,i]
			#id where replacements should go 
			obj[na.id[,i]]<-fill[i]
			obj
		}))
		
		dimnames(fixed)<-dimnames(data)
		
		if(report == TRUE) {
			row.missing <- matrix(round(apply(na.id,1,sum)/ncol(data)*100,0), ncol=1)
			col.missing <- t(matrix(round(apply(na.id,2,sum)/nrow(data)*100,0), nrow =1 ))
			colnames(row.missing)<-colnames(col.missing) <- "percent missing"
			rownames(row.missing)<-rownames(data)
			rownames(col.missing)<-colnames(data)
			# out<-cbind(c(NA,row.missing),rbind(col.missing,fixed))
			#rownames(out)[1]<-colnames(out)[1]<-"percent missing"
			return(list(imputed.data =fixed, prct.missing.in.row=row.missing, prct.missing.in.column=col.missing))
			} else {
				return(fixed)
			}
	}

#get non-redundant ratio of all variables relative to each other
all.ratios<-function(data){
	if(is.null(colnames(data))){colnames(data)<-c(1:ncol(data))}
	#accesory function
	all.pairs <- function(r)
  		list(first = rep(1:r,rep(r,r))[lower.tri(diag(r))],
       second = rep(1:r, r)[lower.tri(diag(r))])
	id<-all.pairs(ncol(data)) 
	den<-split(id$second,id$first)
	num<-split(id$first,id$first)
	vars<-lapply(1:length(num),function(i){
		obj<-as.matrix(data[,num[[i]][1],drop=FALSE])
		tmp<-sweep(as.matrix(data[,den[[i]],drop=FALSE]),2,obj,"/")
		names<-paste0(colnames(data)[den[[i]]],"_ratio_",colnames(data)[num[[i]]])
		colnames(tmp)<-names 
		tmp
	}) 
	
	do.call("cbind",vars)	   
}	

#remove minimum values through addition
make.positive<-function(obj){
	mins<-apply(obj,2,min,na.rm=TRUE)
	sweep(obj,2,abs(mins),"+")
}

#merging data sets
#---------------------------------------
#wrapper for merge
##merge two data sets based on a common index column
## all unique levels of the index are preserved in the final object
## i.e. no variables are dropped
#issues: if variables are duplicated in data sets with same row ids then incorrect merge of data occurs
merge.data.cube<-function(data1,data2,ID="merge.ID",col.meta="col.metadata",row.meta="row.metadata"){
	# do full outer join on data
	# base dimnames for merge on ID name in row- and col meta data
	# do similar merge on the meta data
	
	#TODO
	#if some variable is present for the same sample then need to default to data1 value
	
	#need to make a simple common colname ID which is shared between the two data sets
	full<-unique(c(fixlc(data1[[col.meta]][ ,ID]),fixlc(data2[[col.meta]][ ,ID])))
	tmp.id<-data.frame(id=1:length(full))
	rownames(tmp.id)<-full
	tmp.id2<-data.frame(id=full) #to resort columns
	
	#merge fails with non numeric row ids?
	full<-unique(c(fixlc(data1[[row.meta]][ ,ID]),fixlc(data2[[row.meta]][ ,ID])))
	rtmp.id<-data.frame(id=1:length(full))
	rownames(rtmp.id)<-full
	rtmp.id2<-data.frame(id=full) #to resort columns
	
	#recode dimensions of both data sets based on row and col id
	df1<-data.frame(data1$data)
	dimnames(df1)<-list(fixlc(rtmp.id[data1[[row.meta]][ ,ID],]),fixlc(tmp.id[fixlc(data1[[col.meta]][ ,ID]),]))
	df2<-data.frame(data2$data)
	dimnames(df2)<-list(fixlc(rtmp.id[data2[[row.meta]][ ,ID],]),fixlc(tmp.id[fixlc(data2[[col.meta]][ ,ID]),]))
	
	#add row ID to make sure row meta is in the correct order
	df1$._merge_tmp_ID<-fixlc(data1[[row.meta]][ ,ID])
	df2$._merge_tmp_ID<-fixlc(data2[[row.meta]][ ,ID])
	
	#full outer join on the data
	merged.data<-merge(df1,df2,all=TRUE,sort=FALSE)
	id<-colnames(merged.data)%in%"._merge_tmp_ID"
	row.id<-merged.data[,id]
	merged.data<-merged.data[,!id]
	
	#row meta
	.row.meta<-merge(data1[[row.meta]],data2[[row.meta]],all=TRUE,sort=FALSE)
	rownames(.row.meta)<-make.unique(fixlc(.row.meta[,ID]))
	.row.meta<-.row.meta[row.id,]
	
	#column meta
	c1<-data1[[col.meta]]
	rownames(c1)<-data1[[col.meta]][,ID]
	c2<-data.frame(data2[[col.meta]])
	rownames(c2)<-make.unique(fixlc(data2[[col.meta]][,ID]))
	.col.meta<-merge(c1,c2,all=TRUE)
	rownames(.col.meta)<-make.unique(fixlc(.col.meta[,ID]))

	#format for return
	tmp<-list(merged.data,.row.meta,.col.meta[fixlc(tmp.id2[fixln(colnames(merged.data)),]),])
	names(tmp)<-c("data",row.meta,col.meta)
	return(tmp)
}

# merge similar column names
# filling in each others missing values
col.merge.na<-function(obj,distance=1){
	# convert to matrix to avoid dealing with factor
	
	obj<-tmp.obj<-as.matrix(obj)
	
	#loop on column number 
	#exit loop when all non-unique columns are merged
	# inputting missing values in the first instance with non-missing values in the next
	res<-list()
	name<-colnames(obj)
	watcher<-1:length(name)
	i<-1
	while(length(watcher)>0){
		name<-colnames(tmp.obj)
		watcher<-1:length(name)
		if(is.null(distance)){ 
				id<-grep(name[1],name[-1],ignore.case = TRUE)+1  # account for avoiding self match
			} else {
				id<-agrep(name[1],name[-1],max.distance=distance,ignore.case = TRUE)+1  # account for avoiding self match
		}	
		if(length(id)==0) id<-1
		if(id>1) {
			out<-tryCatch(matrix(merge.na(tmp.obj[,1],tmp.obj[,id]),,1) ,error=function(e) {data.frame(rep("error",nrow(obj)))}) # >2 column merges not supported
			colnames(out)<-name[1]  
			fixed<-c(watcher[1],id)  
		} else {
			out<-obj[,i,drop=FALSE]
			fixed<-watcher[1]
		}
		res[[i]]<-as.matrix(out)
		watcher<-watcher[!watcher%in%fixed]
		tmp.obj<-tmp.obj[,watcher,drop=FALSE]
		# cat(paste(c("DROP--> ",colnames(obj)[(!colnames(obj)%in%colnames(tmp.obj))],"\n"),collapse=", "))
		# print(c("HAVE--> ",sapply(res,colnames,"\n")))
		i<-i+1
	}
	
	return(data.frame(do.call("cbind",res)))
	
}

#merge two vectors filling in NAs
merge.na<-function(obj1,obj2){
	tmp<-fixlc(obj1)
	tmp[is.na(tmp)]<-fixlc(obj2)[is.na(tmp)]
	tmp
}

#wrapper for for multiple column merge.na
multiple.merge.na<-function(obj,name.char="_",prefix="merged"){
	# obj is a data.frame
	# name.char and prefix are used to construct a name for the merged object
	tmp.obj<-obj
	while(ncol(tmp.obj)>1){
		tryCatch(tmp.obj[,1]<-matrix(merge.na(fixlc(tmp.obj[,1]),fixlc(tmp.obj[,2]))), error=function(e){})
		tmp.obj<-tmp.obj[,-2,drop=FALSE]
	}	
	
	#try to make a column name
	# break on name and take the last element
	tmp.n<-unlist(strsplit(colnames(tmp.obj), name.char))
	colnames(tmp.obj)<-paste0(prefix,name.char,tmp.n[length(tmp.n)])
	return(tmp.obj)
}

#trim trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#tests
test<-function(){
#simulate data
data<-matrix(1:5,5,5)
(x<-scale.data(data, type="uv", dim=2))
apply(x,2,sd)

(x<-scale.data(data, type="pareto", dim=2))
apply(x,2,sd)


#make positive
data<-matrix(rnorm(100,10),5,5)
make.positive(data)

make.positive<-function(obj){
	mins<-apply(obj,2,min,na.rm=TRUE)
	sweep(obj,2,abs(mins),"+")
}


val<-apply(data,1,sum)
x<-sweep(data,1,val,"/")
apply(x,1,sum)

x<-scale.data2(data, type="sum", dim=2)

data(mtcars)
data<-mtcars
data$am<-factor(data$am)
(x<-scale.data(data, type="uv", dim=2))


#merge.data.cube
#---------------------
#data
df1<-data.frame(matrix(1:10,10,10)) 
df2<-data.frame(matrix(10:20,10,10))
df3<-data.frame(matrix(c(20:30),10,10))
#col meta
df1.col<-cbind(merge.ID=c(1:5,11:15),info=1)
df2.col<-cbind(merge.ID=c(1:5,21:25),info=2,info2=3)
df3.col<-cbind(merge.ID=c(1:5,26:30),info=2,info2=3)
#rowmeta
df1.row<-cbind(merge.ID=c(1:10),info=1,info2=3)
df2.row<-cbind(merge.ID=c(11:20),info=2)
df3.row<-cbind(merge.ID=c(21:30),info=2)
#create data cube
data1<-list(data=df1,row.metadata=df1.row,col.metadata=df1.col)
data2<-list(data=df2,row.metadata=df2.row,col.metadata=df2.col)
data3<-list(data=df3,row.metadata=df3.row,col.metadata=df3.col)

x<-tryCatch(merge.data.cube(data1,data2,ID="merge.ID"),error=function(e) {cat("merge.data.cube failed\n")}) # add error report info

merge.data.cube(data1=x,data2=data3,ID="merge.ID")

#multiple merge.na
#----------------------
obj<-matrix(1:10, 10,10,byrow=TRUE)
set.seed(123)
obj[sample(1:100,75)]<-NA
colnames(obj)<-paste0(1:ncol(obj),"_variable")

tryCatch(multiple.merge.na(obj),error=function(e){cat("multiple.merge.na<-->FAILED\n")})
#testing normalizations
data(mtcars)
library(car)
#raw
dat<-mtcars
boxplot(dat)

sdat<-scale(dat,center=TRUE,scale=FALSE)
boxplot(sdat)

sdat2<-scale.data(dat, type="mean", dim=2, positive.only=FALSE)
boxplot(sdat2)

#UV
sdat3<-scale.data(dat, type="uv", dim=2, positive.only=FALSE)
boxplot(sdat3)
boxplot(scale(dat,center=FALSE,scale=apply(dat, 2, sd, na.rm = TRUE)))
x<-scale(dat,center=FALSE,scale=TRUE)
apply(x,2,sd)
}
