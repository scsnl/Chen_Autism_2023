##Version 1.9.1#####

##load required packages
packages <- c("Hmisc", "reshape2", "ggplot2", "psych", "Rmisc", "car",
                "GPArotation","stringr","corrplot")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

invisible(lapply(packages,require,c=T)) #invisible suppressed the output
###Setting up some parameters

statsRD<-2 ##decimal place for means, std, etc
pvalRD<-3 ###decimal place for p value

###########


###group different t-table  ##Inferential test
##group t-tests
mk_ttable<-function (data,pgrpv,grplabels,stp,filehead,statsRD,pvalRD,equalvar=T) { 
  #pgrpv: column number for the group variable to compare 
  #equalvar: 1=equal variance; 0 = unequal variance
  #grplabels: a character vector with two elements to specify the two subgroups to compare
  ###drop unused levels of the group variable
  data<-data[(data[,pgrpv]==grplabels[1] |data[,pgrpv]==grplabels[2]),]
  ##drop the unselected levels
  data[,pgrpv]<-factor(data[,pgrpv])
  stp<-stp   #the starting number of continuous variables
	range<-stp:ncol(data)
	if(equalvar) {
		x<-lapply(data[,range], function(x) t.test(x~data[,pgrpv],var.equal=T,conf.level=.95))
	} else {
		x<-lapply(data[,range], function(x) t.test(x~data[,pgrpv],var.equal=F,conf.level=.95))
	}
	sd_grp1<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[1],range],sd,na.rm=T))
	mean_grp1<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[1],range],mean,na.rm=T))
	sd_grp2<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[2],range],sd,na.rm=T))
	mean_grp2<-unlist(lapply(data[data[,pgrpv]==levels(data[,pgrpv])[2],range],mean,na.rm=T))
	t.table<-matrix(,length(x),7) #create an empty matrix to save the t-tests results for all variables, there are 7 values, t, df, p, ci.lower, ci.upper, group1mean,group2mean
	##put mean and sd
	##if mean and sd are smaller than 0.005, they will be rounded to 0.00, so change to scienfic display if so
	t.table[,1]<-paste0(ifelse(abs(mean_grp1)>0.005,round(mean_grp1,digits=statsRD),
	                           formatC(mean_grp1,format = "e",digits=statsRD))," (",
	                    ifelse(abs(sd_grp1)>0.005,round(sd_grp1,digits=statsRD),formatC(sd_grp1,format = "e",digits=statsRD)),")")
	t.table[,2]<-paste0(ifelse(abs(mean_grp2)>0.005,round(mean_grp2,digits=statsRD),
	                           formatC(mean_grp2,format = "e",digits=statsRD))," (",
	                    ifelse(abs(sd_grp2)>0.005,round(sd_grp2,digits=statsRD),formatC(sd_grp2,format = "e",digits=statsRD)),")")
	for (i in 1:nrow(t.table)) {
		t.table[i,3]<-round(x[[i]]$statistic,digits=statsRD)
		t.table[i,4]<-x[[i]]$parameter
		t.table[i,5]<-ifelse(round(x[[i]]$p.value,digits=pvalRD)<0.001,"<.001",round(x[[i]]$p.value,digits=pvalRD))
		t.table[i,6]<-round(x[[i]]$conf.int[1],digits=statsRD)
		t.table[i,7]<-round(x[[i]]$conf.int[2],digits=statsRD)
	}
	
	colnames(t.table)<-c(paste0("Mean (SD) in ",levels(data[,pgrpv])[1]),
	                     paste0("Mean (SD) in ",levels(data[,pgrpv])[2]),
	                     "t-value","df","p-value","lower.ci","upper.ci")
	rownames(t.table)<-colnames(data[,range])
	
	##save t-tests results
	grpname<-levels(data[,pgrpv])
	write.csv(t.table,file=paste0(filehead,"_",grpname[1],"_vs_",grpname[2],"comp_t-test_summary_table.csv"),quote=F,row.names=T)
}

####make descriptive data summary table
mk_destable<-function (data,pgrpv,stp,filehead,statsRD) {  #pgrpv: column number for group variable; if equals to 0, no stats separate by group will be done
    stp<-stp   #the starting number of continuous variables
	drange<-stp:ncol(data)
	###descriptive data
	if (pgrpv == 0) {
	  sd_grp<-unlist(lapply(data[,drange],sd,na.rm=T))
	  sd_grp<-ifelse(abs(sd_grp)>0.005,round(sd_grp,statsRD),formatC(sd_grp,format = "e",digits = statsRD))
	  mean_grp<-unlist(lapply(data[,drange],mean,na.rm=T))
	  mean_grp<-ifelse(abs(mean_grp)>0.005,round(mean_grp,statsRD),formatC(mean_grp,format = "e",digits = statsRD))
	  min_grp<-unlist(lapply(data[,drange],min,na.rm=T))
	  max_grp<-unlist(lapply(data[,drange],max,na.rm=T))
	  range_grp<-paste(ifelse(abs(min_grp)>0.005,round(min_grp,statsRD),formatC(min_grp,format = "e",digits = statsRD)),
	                   ifelse(abs(max_grp)>0.005,round(max_grp,statsRD),formatC(max_grp,format = "e",digits = statsRD)),sep = "-")
	  n_grp<-unlist(lapply(data[,drange], function(x) sum(!is.na(x))))
	  summ_grp<-cbind(colnames(data[,drange]),n_grp,paste0(mean_grp," (",sd_grp,")"),range_grp)
	  colnames(summ_grp)<-c("variable","n","mean (SD)","range")
		write.csv(summ_grp,file=paste0(filehead,"_whole_sample_descriptive.csv"),row.names = F)
	} else {
		
		for (i in 1:length(levels(data[,pgrpv]))) {
		  sub_data<-data[data[,pgrpv]==levels(data[,pgrpv])[i],]
		  sd_grp<-unlist(lapply(data[,drange],sd,na.rm=T))
		  sd_grp<-ifelse(abs(sd_grp)>0.005,round(sd_grp,statsRD),formatC(sd_grp,format = "e",digits = statsRD))
		  mean_grp<-unlist(lapply(data[,drange],mean,na.rm=T))
		  mean_grp<-ifelse(abs(mean_grp)>0.005,round(mean_grp,statsRD),formatC(mean_grp,format = "e",digits = statsRD))
		  min_grp<-unlist(lapply(data[,drange],min,na.rm=T))
		  max_grp<-unlist(lapply(data[,drange],max,na.rm=T))
		  range_grp<-paste(ifelse(abs(min_grp)>0.005,round(min_grp,statsRD),formatC(min_grp,format = "e",digits = statsRD)),
		                   ifelse(abs(max_grp)>0.005,round(max_grp,statsRD),formatC(max_grp,format = "e",digits = statsRD)),sep = "-")
		  n_grp<-unlist(lapply(sub_data[,drange], function(x) sum(!is.na(x))))
		  summ_grp<-cbind(colnames(data[,drange]),n_grp,paste0(mean_grp," (",sd_grp,")"),range_grp)
		  colnames(summ_grp)<-c("variable","n","mean (SD)","range")
			write.csv(summ_grp,file=paste0(filehead,"_",levels(data[,pgrpv])[i],"_group_descriptive.csv"),row.names = F)
		}	
	}
}


###Correlation matrix
mk_rtable<-function (data,stp,statsRD,filehead) {
    data_temp<-data[,(stp:ncol(data))]
	r.mat<-corr.test(data_temp)
	r.table<-r.mat$r
	r.table<-round(r.table,digits=statsRD)
	p<-r.mat$p
	p[is.na(p)]<-1
	r.table[p<0.05]<-paste0(r.table[p<0.05],"*")
	r.table[p<0.01]<-paste0(r.table[p<0.01],"*")
	r.table[p<0.001]<-paste0(r.table[p<0.001],"*")
	r.table[(p<.10 & p>.05)]<-paste0(r.table[(p<.10 & p>.05)],"#")
	#r.table<-matrix(r.table,ncol(data_temp),ncol(data_temp),byrow=T)
	r.table[upper.tri(r.table)]<-""
	diag(r.table)<-"-"
	colnames(r.table)<-colnames(data_temp)
	rownames(r.table)<-colnames(data_temp)
	write.csv(r.table, file=paste0(filehead,"_correlation_matrix.csv"),quote=F,row.names=T)
}


						  