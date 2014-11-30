#' Extract the odds ratios from a S3 object of glm 
#' 
#' Extract the odds ratios from a S3 object of glm 
#' @param x A S3 object of glm
#' @param digits An integer indicating the number of decimal places (round) or
#'               significant digits (signif) to be used. Default value is 2. 
#' @return A data.frame consist of odds ratios and 95% confidence interval and 
#'         p values
extractOR=function(x,digits=2){
    suppressMessages(a<-confint(x))
    result=data.frame(exp(coef(x)),exp(a))
    result=round(result,digits)
    result=cbind(result,round(summary(x)$coefficient[,4],3))
    colnames(result)=c("OR","lcl","ucl","p")
    result
}


#' Plot for odds ratios for a S3 object of glm
#'
#' Plot for odds ratios for a S3 object of glm 
#' @param x A S3 object of glm
#' @param type an integer defining the shape of plots; default value is 1
#' @param xlab label for the horizontal-axis; defaults to "Odds Ratios" 
#' @param ylab label for the vertical axis; defaults to "".    
#' @param show.OR A logical value; Whether or not show p values on plot
#' @param show.CI A logical value; Whether or not show 95\% CI values on plot
#' @param sig.level A numeric value of upper limit of p value of showing variables
#' @param cex A numerical value giving the amount by which plotting OR/HR symbols 
#'            should be magnified relative to the default, defaulting 1.2. 
#' @param lwd The line width, a positive number, defaulting to 2.        
#' @param pch Either an integer specifying a symbol or a single character 
#'           to be used as the default in plotting OR/HR points.   
#' @param col A specification for the default plotting color.
#' @param ... arguments to be passed to plot  
#' @return This function return NULL invisibly and draw graphs
#' @examples
#' require(survival)
#' data(colon)
#' out1=glm(status~sex+age+rx+obstruct+node4,data=colon)
#' out2=glm(status~rx+node4,data=colon)
#' ORplot(out1,type=2,show.CI=TRUE,xlab="This is xlab",main="Main Title")
#' ORplot(out2,type=1,main="Main Title")
#' ORplot(out1,type=2,show.CI=TRUE,main="Main Title")
#' ORplot(out1,type=3,show.CI=TRUE,main="Main Title",sig.level=0.05)
#' ORplot(out1,type=3,show.CI=TRUE,main="Main Title",sig.level=0.05,
#'        pch=1,cex=2,lwd=4,col=c("red","blue"))
ORplot=function(x,type=1,xlab="",ylab="",show.OR=TRUE,show.CI=FALSE,
                sig.level=1,cex=1.2,lwd=2,pch=18,col=NULL,...){
    result=extractOR(x)
    ORplot.sub(result[-1,],type,xlab,ylab,show.OR,show.CI,sig.level,
               cex=cex,lwd=lwd,pch=pch,col=col,...)
}

#' A sub function for ORplot anf HRplot
#' 
#' Plot for odds ratios for a S3 object of glm
#' @param result A resultant data.frame of function extractOR
#' @param type an integer defining the shape of plots; default value is 1
#' @param xlab label for the horizontal-axis; defaults to "Odds Ratios" 
#' @param ylab label for the vertical axis; defaults to "".    
#' @param show.OR A logical value; Whether or not show p values on plot
#' @param show.CI A logical value; Whether or not show 95\% CI values on plot
#' @param sig.level A numeric value of upper limit of p value of showing variables
#' @param cex A numerical value giving the amount by which plotting OR/HR symbols 
#'            should be magnified relative to the default, defaulting 1.2. 
#' @param lwd The line width, a positive number, defaulting to 2.        
#' @param pch Either an integer specifying a symbol or a single character 
#'           to be used as the default in plotting OR/HR points.   
#' @param col A specification for the default plotting color.
#' @param ... Further arguments to be passed to plot
#' @return This function return NULL invisibly and draw graphs
ORplot.sub=function(result,type=1,xlab="",ylab="",show.OR=TRUE,show.CI=FALSE,
                    sig.level=1,cex=1.2,lwd=2,pch=18,col=NULL,...){    
    result=result[result[[4]]<=sig.level,]
    count=length(result[,1])
    if(count<1) {
        cat("No variable to be plotted found")
        return(invisible())
    }    
    if(is.null(col) | (length(col)!=2) ) {
        if(type==3) {col1="salmon";col2="darkturquoise"}
        else {col1="firebrick2";col2="dodgerblue3"}
    } else {
        col1=col[1];col2=col[2]
    }
    result=result[order(result[[1]],decreasing=TRUE),]
    
    max=max(result[,-4],na.rm=T)+0.1
    min=min(result[,-4],na.rm=T)
    
    x=log10(result[,1])*5/log10(max)
    x1=log10(result[,2])*5/log10(max)
    x2=log10(result[,3])*5/log10(max)
    opar<-par(no.readonly=TRUE)
    if(show.CI) par(mar=c(5,8,4,8))
    else par(mar=c(5,8,4,2))
    if(xlab=="") xlab=ifelse(colnames(result)[1]=="OR","Odds Ratios","Harzard Ratios")
    plot(result[,1],count:1,type="n",axes=FALSE,ylim=c(0.5,count+0.5),
         xlim=c(log10(min)*5/log10(max),5),xlab="",ylab=ylab,...)         
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], border=FALSE,
         col = "gray88")
         
    abline(h=count:1,col="white",lwd=2)
    abline(v=0,lty=2,col="darkgray",lwd=2)
    range=seq(min,max,0.5)
    range2=seq(min+0.25,max,0.5)
    abline(v=log10(range2)*5/log10(max),col="white",lwd=1)
    abline(v=log10(range)*5/log10(max),col="white",lwd=2)
    yscale=par("usr")[3:4]
    xscale=par("usr")[1:2]
    text(x=log10(range)*5/log10(max),y=par("usr")[3],range,pos=1,cex=0.8,xpd=TRUE)
    if(length(range2)<4) text(x=log10(range2)*5/log10(max),y=par("usr")[3],range2,pos=1,cex=0.6,xpd=TRUE)
    text(y=count:1,par("usr")[1],labels=rownames(result),pos=2,cex=0.9,xpd=TRUE)
    #text(x=5,y=par("usr")[3],xlab,pos=1,cex=1,xpd=TRUE)
    text(x=mean(range(xscale)),y=par("usr")[3],xlab,pos=1,cex=1,offset=2,xpd=TRUE)
    p=c()
    for(i in 1:count){
        if(is.nan(result[i,4])) p[i]=result[i,1]
        else if(result[i,4]<0.001) p[i]=paste(result[i,1],"***",sep="")
        else if(result[i,4]<0.01) p[i]=paste(result[i,1],"**",sep="")
        else if(result[i,4]<0.05) p[i]=paste(result[i,1],"*",sep="")
        else p[i]=result[i,1]
    }
    if(type<3){
        segments(x1,count:1,x2,count:1,lty=1,
                 col=ifelse(x>0,col1,col2),lwd=lwd)
        if(show.OR) text(x,count:1,p,pos=3,cex=0.8)
        if(type==2) {
            segments(x1,(count:1)-0.1,x1,(count:1)+0.1,lty=1,
                             col=ifelse(x>0,col1,col2),lwd=lwd)
            segments(x2,(count:1)-0.1,x2,(count:1)+0.1,lty=1,
                     col=ifelse(x>0,col1,col2),lwd=lwd)
            points(x,count:1,col=ifelse(x>0,col1,col2),pch=pch,cex=cex)
        } else{
            points(x,count:1,col=ifelse(x>0,col1,col2),pch=pch,cex=cex)
            
        }   
        
    }
    else{
        left=ifelse(x>0,0,x)
        right=ifelse(x>0,x,0)
        height=0.25
        rect(left,(count:1)+height,right,(count:1)-height,
             col=ifelse(x>0,col1,col2))
        segments(x1,count:1,x2,count:1,lty=1,lwd=lwd)
        if(show.OR) text(0,count:1,p,pos=ifelse(x>0,2,4),cex=0.8)
    }
    if(show.CI){
        text(y=count:1,par("usr")[2],
            paste(p," (",result[,2],"-",result[,3],")",sep=""),
            pos=4,xpd=TRUE) 
        CI.title=paste(colnames(result)[1],"(95% C.I.)")
        text(y=max(yscale),par("usr")[2],CI.title,pos=4,xpd=TRUE)
    }    
    par(opar)
}

