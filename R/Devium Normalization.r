
# data overview and normalization functions
#function to calculate within and between batch precision for all variables
calc.mRSD<-function(data,batch=data.frame(1:nrow(data)),summary.range=seq(0,100,10),use="mean"){
	library(plyr)
	
	#bin summaries into range
	if(!is.factor(batch)){batch<-tryCatch(factor(batch[,],levels=unique(batch[,])),error=function(e){factor(batch,levels=unique(batch))}); message("Batch converted to factor.")}
	
	#main object
	tmp<-data.frame(batch=batch,data)
	
	#parametric summary
	b.m<-ddply(tmp,.(batch),colwise(mean))
	b.s<-ddply(tmp,.(batch),colwise(sd))
	b.rsd<-abs(b.s/b.m*100) # ignore negative and positive RSDs due to mean
	b.rsd[,1]<-b.m[,1]
	
	
	# #non-parametric summary
	# b.m2<-ddply(tmp,.(batch),colwise(median))
	# b.s2<-ddply(tmp,.(batch),colwise(IQR))
	# b.rsd2<-b.s2/b.m2*100
	# med.rsd.np<-apply(b.rsd2[,-1],1,median,na.rm=T) # 
	
	#generate summary objects for analytes between all batches
	analyte.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],2,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],2,sd,na.rm=T)) 
	colnames(analyte.RSD)[1]<-use# RSD for variables over all batches
	analyte.RSD.summary<-split.bins(obj=analyte.RSD[,1],bins=seq(0,100,10)) # summary for variables over all batches
	analyte.RSD.summary$percent<-round(analyte.RSD.summary$count/sum(analyte.RSD.summary$count)*100,1)
	
	#generate summary objects for batches based on all analytes
	within.batch.RSD<-data.frame(mean=apply(b.rsd[,-1,drop=F],1,use,na.rm=T), sd=apply(b.rsd[,-1,drop=F],1,sd,na.rm=T))
	rownames(within.batch.RSD)<-b.rsd[,1]
	colnames(within.batch.RSD)[1]<-use
	within.batch.RSD.summary<-split.bins(na.omit(within.batch.RSD[,1]),bins=seq(0,100,10)) # ,max(within.batch.RSD[,1]
	within.batch.RSD.summary$percent<-round(within.batch.RSD.summary$count/sum(within.batch.RSD.summary$count)*100,1)
	
	#return summary
	list(batch.means=b.m,batch.sd=b.s,all.batch.RSD=b.rsd,variable.RSD=analyte.RSD,batch.RSD=within.batch.RSD,variable.RSD.summary=analyte.RSD.summary,batch.RSD.summary=within.batch.RSD.summary)	

}

#create summary objects from object produced by calc.mRSD
summarize.performance<-function(obj=gc.perf.raw,sig.figs=2){
	#optionally bin objects into an interval range
	
	#batch summary
	batch.med.perf<-data.frame(median.RSD=signif(median(obj$batch.RSD[,1],na.rm=TRUE),sig.figs), range=paste(signif(range(obj$batch.RSD[,1],na.rm=TRUE),sig.figs),collapse=", "))
	#clean up batch summary
	tmp<-obj$batch.RSD.summary
	tmp2<-gsub("\\[","",gsub(",","-",gsub("\\]","",gsub("\\(","",fixlc(tmp[,1])))))
	batch.summary<-data.frame(RSD=tmp2,count=tmp[,2],percent=round(tmp[,3],0))
	batch.summary$cumulative.percent<-cumsum(batch.summary$percent)
	#analyte summary
	var.med.perf<-data.frame(median.RSD=signif(median(obj$variable.RSD[,1],na.rm=TRUE),sig.figs), range=paste(signif(range(obj$variable.RSD[,1],na.rm=TRUE),sig.figs),collapse=", "))
	#clean up analyte summary
	tmp<-obj$variable.RSD.summary
	tmp2<-gsub("\\[","",gsub(",","-",gsub("\\]","",gsub("\\(","",fixlc(tmp[,1])))))
	var.summary<-data.frame(RSD=tmp2,count=tmp[,2],percent=round(tmp[,3],0))
	var.summary$cumulative.percent<-cumsum(var.summary$percent)
	
	
	
	#return
	list(batch=batch.med.perf,batch.summary=batch.summary,variable=var.med.perf,variable.summary=var.summary)
}

#split vector based on probabilities and calculate counts
split.prob<-function(obj,probs=seq(0, 1, 0.25)){
	library(plyr)
	splits<-quantile(obj,probs)
	interval<-cut(obj,splits,include.lowest = TRUE)
	tmp<-data.frame(count=obj,interval=interval)
	ddply(tmp,.(interval),colwise(length))
}

#split object on bins
split.bins<-function(obj,bins=seq(10,100,10)){
	library(plyr)
	
	interval<-cut(obj,bins,include.lowest = TRUE)
	tmp<-data.frame(count=obj,interval=interval)
	tmp<-ddply(tmp,.(interval),colwise(length))
	#need a mechanism to group values over largest bin
	int<-fixlc(tmp$interval)
	int[is.na(int)]<-paste0(">",max(bins))
	tmp$interval<-int
	tmp
}

#identify samples with > quantile
over.prob<-function(val=tmp.data$leverage,prob=0.95){
	val>quantile(val,probs=prob)
}

#adjust data by batch scalar (add ability to get ratios using QCs and adjust samples)
scalar.batch.adjust<-function(obj,factor,use="median",train=NULL){
	#calculate ratio of median of each factor level to global median
	#return adjusted value and adjustment for each factor (should be numeric else levels could be broken)
	# train can be a logical specifying which subset of the data to use to calculate the ratios
	library(plyr)
	if(!class(obj)=="data.frame"){obj<-as.data.frame(obj)}
	if(is.logical(train)){
		full<-data.frame(obj)
		split.data<-split(full,factor(train))
		train.data<-split.data$"TRUE"
		train.factor<-unlist(split(as.data.frame(factor),factor(train))$"TRUE")
	} else {
		train.data<-obj
		train.factor<-factor	
	}
	
	#remove outliers
	
	
	global.med<-apply(train.data,2,use,na.rm=TRUE)
	#main object
	tmp<-data.frame(batch=train.factor,train.data)
	
	#summary for all batches and analytes
	b.m<-ddply(tmp,.(batch),colwise(use))
	med.ratio<-sweep(b.m[,-1,drop=F],2,unlist(global.med),"/")
	
	#return ratio adjusted data
	big.l<-split(obj,factor)

	res<-lapply(1:length(big.l),function(i){
		tmp<-big.l[[i]]
		rat<-unlist(med.ratio[i,])
		res<-sweep(tmp,2,rat,"/")
		res[is.na(res)]<-0 # for ratios with zero
		res
	})
	adjusted.data<-do.call("rbind",res)
	#clean of strange vars
	adjusted.data[adjusted.data=="Inf"]<-NA
	
	list(adjusted.data=adjusted.data,ratios=med.ratio)
}

#plot a single variable line plot
summary.lineplot<-function(val,groups=NULL,view.split=NULL,theme=NULL,se=FALSE,extra=NULL,span=0.75,print.plot=TRUE){
	library(ggplot2)
	#data should minimally contain a single variable of interest(val) and additionally factor identifying groups 
	vis.data<-data.frame(value=unlist(unname(val)))
	vis.data$id<-1:length(vis.data$val)
	
	if(is.null(groups)){vis.data$groups<-1;vis.data$color<-1} else {vis.data$groups<-factor(as.matrix(groups))}
	if(is.null(view.split)){
		add.facet<-NULL
	} else {
			vis.data$facet<-vis.data$groups
		if(view.split=="y"){
			add.facet<-facet_grid(facet ~ .)
		} else {
			add.facet<-facet_grid( . ~ facet)
		}	
	}
	
	p<-ggplot(data=vis.data,aes(y=value,x=id)) + geom_point(aes(color=groups),alpha=.75,show_guide=FALSE)+
	stat_smooth(aes(group=groups,color=groups),method = "loess", size = 1,se=se,alpha=.1,span=span) + theme + add.facet + xlab(colnames(groups))+ylab(colnames(val))+ 
	guides(col = guide_legend(title = colnames(groups))) + extra
	if(print.plot){
		print(p)
	} else {
		return(p)
	}	
}

#box plot for 2 factors with loess smoothing
summary.boxplot2<-function(val,groups=NULL,split.on=NULL,theme=NULL,se=FALSE,span=0.75,extra=NULL,print.plot=TRUE){
	#data should minimally contain a single variable of interest(val) and additionally factor identifying groups 
	library(ggplot2)
	vis.data<-data.frame(value=unlist(val))

	if(is.null(groups)){
		vis.data$groups<-1;vis.data$color<-1
	} else {
		vis.data$groups<-factor(as.matrix(groups))
	}
	
	if(is.null(split.on)){
		vis.data$split.on<-""
		l.guide<-NULL
		# extra<-scale_fill_manual(values ="grey50")
		smooth<-NULL
	} else {
		vis.data$split.on<-factor(as.matrix(split.on))
		l.guide<-guides(fill = guide_legend(title = colnames(split.on)))
		# extra<-NULL
		smooth<-stat_smooth(aes(group=split.on,color=split.on),method = "loess", size = 1.25,se=se,alpha=.1,show_guide=FALSE,span=span)
	}
	
	p<-ggplot(data=vis.data,aes(y=value,x=groups)) + geom_boxplot(aes(fill=split.on),alpha=.75) +
	smooth + theme + xlab(colnames(groups))+ ylab(colnames(val))+ l.guide 
	p<-p+extra
	
	if(print.plot){
		print(p)
	} else {
		return(p)
	}	
}

#create summary plot RSD% analyte mean
RSD.means.plot<-function(obj=list(gc.perf.raw,gc.raw.t1),type="variable",name=c("Raw","FAME L2 norm"),size=3,alpha=.75,use.log=TRUE,se=FALSE,points=TRUE,theme=NULL,extra=NULL,label=FALSE,label.size=2,span=.75){
	
	library(ggplot2)
	#check if many or single object
	if(is.data.frame(obj)){obj<-list(obj)} 
	
	#obj can be a list of objects produced by calc.mRSD
	#name will be used for legends
	#switch between samples and variables
	res<-lapply(1:length(obj),function(i){
		tmp<-obj[[i]]	
		#switch between samples and variables
		if(type=="variable"){	
			res<-data.frame(method=name[i],RSD=tmp$variable.RSD[,1],mean=apply(tmp$batch.means[-1],2,mean,na.rm=TRUE),labels=rownames(tmp$variable.RSD))
		} else {
			#for samples the mean is the sum of all variables
			res<-data.frame(method=name[i],RSD=tmp$batch.RSD[,1],mean=apply(tmp$batch.means[-1],1,sum,na.rm=TRUE),labels=rownames(tmp$batch.means))
		}	
		res$log.mean<-log(res$mean+1)
		res
	})
	
	if(type=="variable"){
		y.lab<-"Mean"
	} else{
		y.lab<-"Sum"
	}
	
	lab.data<-if(label) geom_text(aes(label=labels),size=label.size,show_guide=FALSE) else NULL
	
	vis.data<-do.call("rbind",res)
	legend<-TRUE
	if(use.log){
		p<-ggplot(vis.data,aes(x=log.mean,y=RSD,group=method,color=method,fill=method))
		if(points){p<-p+geom_point(alpha=alpha,size=size) ;legend<-FALSE}
		p<-p+stat_smooth(method = "loess", size = 1,show_guide=legend ,se = se,alpha=.75,span=span) +
		theme + xlab(paste0("log ",y.lab))+ ylab("RSD") + lab.data +
		extra #+scale_color_manual(values=rainbow(3))+
		print(p)	
	} else {
		p<-ggplot(vis.data,aes(x=mean,y=RSD,group=method,color=method,fill=method))
		if(points){p<-p+geom_point(alpha=.75) ;legend<-FALSE }
		p<-p + stat_smooth(method = "loess", size = 1,show_guide=legend ,se = se,alpha=.75,span=span)+ 
		theme + xlab(y.lab)+ ylab("RSD") + lab.data +
		extra #+scale_color_manual(values=rainbow(3))+
		print(p)
	}
}	

#bar plot to summarize performance
RSD.counts.plot<-function(obj,show="variable",plot.obj="count",name="",theme=NULL,extra=NULL,ylabel="number of metabolites",barplot=TRUE){
	
	if(show=="variable"){
		#variables
		res<-lapply(1:length(obj),function(i){
			tmp<-obj[[i]]	
			data.frame(method=name[i],tmp$variable.summary)
		})
	} else {
		#samples
		res<-lapply(1:length(obj),function(i){
			tmp<-obj[[i]]	
			data.frame(method=name[i],tmp$batch.summary)
		})
	}	
	
	vis.data<-do.call("rbind",res)
	#hack to get the sort correct
	vis.data<-vis.data[order(fixlc(vis.data$RSD)),]
	fix<-grep(">",vis.data$RSD)
	if(length(fix)>0){
		tmp<-rbind(vis.data[-fix,],vis.data[fix,])
		vis.data<-tmp
	}	
	
	vis.data$interval<-factor(fixlc(vis.data$RSD), levels=unique(fixlc(vis.data$RSD)))

	#switch which variable in the data is plotted
	vis.data$plot.obj<-vis.data[,plot.obj]
	upper<-max(vis.data$plot.obj)
	ulim<-if(upper>=10){10} else {upper}
	dlim<-if(upper>=10){2} else {1}
	p<-ggplot(data=vis.data,aes(x=interval,y=plot.obj,fill=method,group=method))
	if(barplot){
		p<-p+geom_bar(position=position_dodge(),stat="identity") 
	} else {	
		p<-p+geom_bar(position=position_dodge(),stat="identity") 
	}
	p<- p+ theme +
	scale_y_continuous(minor_breaks = seq(0 , upper, dlim), breaks = seq(0, upper, ulim)) + xlab("RSD")+ylab(ylabel)+ extra #scale_fill_brewer(palette="Set1")
	print(p)
}

#conduct LOESS normalization on a data frame or matrix
loess.normalization<-function(x,y,subset=NULL,progress=TRUE,adjust="median",span=0.75,...){

	#subset = logical specifying which subset of the data to be used for fitting
	#adjust =  used to adjust post normalized data statistic to that of the pre normalized
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(x), style = 3)} else {pb<-NULL}
	span<-tryCatch(rep(span,length.out=ncol(x)),error=function(e){span}) # recycle
	res<-do.call("cbind",lapply(1:ncol(x),function(i){	
		tmp.x<-x[,i]
		fit<-loess(tmp.x~y,subset=subset,span=span[i],...)
		pred<-predict(fit,data.frame(tmp.x=tmp.x))
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		return(tmp.x-pred) # residuals for train and test
	}))
	if (progress == TRUE){close(pb)}
	if(!is.null(adjust)){
			scale1<-apply(x,2,adjust,na.rm=TRUE)
			tmp<-sweep(res,2,apply(res,2,min,na.rm=TRUE),"-") # get rid of negative values
			mins<-apply(x,2,min,na.rm=TRUE)
			tmp<-sweep(tmp,2,mins,"+")
			scale2<-apply(tmp,2,adjust,na.rm=TRUE)
			adjustment<-scale1/scale2
			res<-sweep(tmp,2,adjustment,"*")
	}
	return(res)
}

#cross-validation based tuning of LOESS
tune.loess<-function(data,y,folds=7,span.vals=seq(.25,1,by=.1),progress=TRUE){
	# X can be a data frame
	# wrapper for bisoreg::loess.wrapper
	#returns optimal span for each column in X
	library(bisoreg)
	if (progress == TRUE){ pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)} else {pb<-NULL}
	res<-unlist(lapply(1:ncol(data),function(i){
		x<-tryCatch(loess.wrapper(x=data[,i], y=y, span.vals = span.vals, folds = folds)$pars$span, error=function(e){NA})
		if (progress == TRUE){setTxtProgressBar(pb, i)}
		return(x)
	}))
	if (progress == TRUE){close(pb);cat("\n")}
	
	return(res)
}

#calculate RSD
calc.RSD<-function(x,...){sd(x,...)/mean(x,...)}

#calculate correlation between LOESS models for train and test data sets
#use spline interpolation to complete segments prior to correlation testing
#use fxn to test if loess normalization between train/ test set makes any sense
#if train test trend are correlated then loess make sense if not then no
loess.correlation<-function(data,y,train,span=0.75,n=nrow(data),plot=FALSE,cor.method="spearman",...){
	#fit loess models for each variable
	if(plot){par(mfrow=c(ceiling(ncol(data)/ceiling(ncol(data)/4)),ceiling(ncol(data)/4)))}
	do.call("rbind",lapply(1:ncol(data),function(i){
		x1<-data[train,i]
		y1<-y[train]
		x2<-data[!train,i]
		y2<-y[!train]
		fit1<-loess(x1~y1,span=span,...)
		fit2<-loess(x2~y2,span=span,...)
		
	
		#need to interpolate the fits to the same interval 
		#in order to carry out the correlation
		int.fit1<-spline(y1,fit1$fitted, n=n, method = "fmm", xmin =min(c(y1,y2)), xmax = max(c(y1,y2)),...)
		int.fit2<-spline(y2,fit2$fitted, n=n, method = "fmm", xmin =min(c(y1,y2)), xmax = max(c(y1,y2)),...)
		if(plot){
			plot(range(c(y1,y2)),range(c(x1,x2)), type="n",bty="n",xlab="y",ylab=colnames(data)[i])
			lines(int.fit1$x,int.fit1$y,col="red",lwd=2)
			lines(int.fit2$x,int.fit2$y,col="blue",lwd=2)
			legend("topright",col=c("red","blue"),legend=c("train","test"),lwd=2,bty="n")
		}	
		cor<-cor.test(int.fit1$y,int.fit2$y,method=cor.method,...)
		return(data.frame(cor=cor$estimate,p.value=cor$p.value))
		
		
		})
		
	)

}


# #test
# test<-function(){

# #simulate replicated measurements
# sim.replicated<-function(nsamples=10,ngroups=2,nvariables=10,sd.mag=.25,mag=c(1,100),effect=1){
	# var.m<-sample(mag[1]:mag[2],nvariables,replace=TRUE)
	
	# sample.ids<-rep(1:ngroups,each=nsamples)
	# results<-do.call("cbind",lapply(1:nvariables,function(i){
		# v.mean<-var.m[i]
		# g.mean<-seq(v.mean,(v.mean+v.mean*(effect*sd.mag)),length.out=ngroups)
		# unlist(lapply(1:ngroups,function(j){
			# rnorm(nsamples,g.mean[j],g.mean[j]*sd.mag)
		# }))
	# }))
	# colnames(results)<-paste0("var",1:nvariables)
	# data.frame(group=factor(paste0("Batch ",sample.ids)),results)
# }

# #create simulated data
# rep.data<-sim.replicated(nsamples=10,ngroups=10,nvariables=10,sd.mag=.25,mag=c(1,100),effect=.5)
# rep.data<-data.frame(rep.data[,1],scale(rep.data[,-1])

# write.csv(rep.data,"replicated data.csv")
# #plots
# library(ggplot2)
# .theme1<- theme(
			# axis.line = element_line(colour = 'gray', size = .75), 
			# panel.background = element_blank(), 
			# plot.background = element_blank(),
			# axis.text.x = element_text(size = 10),
			# axis.text.y = element_text(size = 10),
			# axis.title.x = element_text(size=15),
			# axis.title.y = element_text(size=15),
			# # axis.text.x = element_blank(),
			# # axis.ticks.x = element_blank(),
			# legend.key = element_blank()
		 # )
# #for boxplots		 
# .theme2<- theme(
			# axis.line = element_line(colour = 'gray', size = .75), 
			# panel.background = element_blank(), 
			# plot.background = element_blank(),
			# axis.text.x = element_blank(),#element_text(size = 10),
			# axis.text.y = element_text(size = 10),
			# axis.title.x = element_text(size=15),
			# axis.title.y = element_text(size=15),
			# # axis.text.x = element_blank(),
			# # axis.ticks.x = element_blank(),
			# legend.key = element_blank()
			# # legend.position="none"
		 # )	

# #prepare data
# rep.id<-factor(rep.data[,1])
# tmp.data<-rep.data[,-1] +20		 
		 
# #%RSD stats for samples and variables
# RSD<-calc.mRSD(data=data.frame(tmp.data),batch=data.frame(rep.id)) # raw data

# #plot % RSD vs. mean scatter plot

# RSD.means.plot(obj=list(RSD),type="variable",name=c("Raw"),theme=.theme1,label=TRUE,use.log=TRUE)
# RSD.means.plot(obj=list(RSD),type="sample",name=c("Raw"),theme=.theme1)

# #create summary objects
# RSD.summary<-summarize.performance(obj=RSD,sig.figs=2)

# #bar plot
# RSD.counts.plot(obj=list(RSD.summary),show="variable",name=c("Raw"),theme=.theme1,ylabel="number of metabolites")
# RSD.counts.plot(obj=list(RSD.summary),show="sample",name=c("Raw"),theme=.theme1,ylabel="number of batches") # control y axis scale_y_continuous(

# #plot trend across batches for a single variable
# #could use z-scores and plot multiple analytes on the same graph
# best<-sample(1:ncol(tmp.data),1)	#select variable to view
# summary.lineplot(val=tmp.data[,best,drop=F],groups=NULL,theme=.theme2)
# summary.lineplot(val=tmp.data[,best,drop=F],groups=data.frame(rep.id),theme=.theme2)

# #boxplot
# summary.boxplot2(val=tmp.data[,best,drop=F],groups=rep.id,split.on=rep.id,theme=.theme2)

# #%RSD stats


# }