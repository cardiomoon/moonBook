#' Perform coxph of individual expecting variables
#'
#' @param formula An object of class "formula". Left side of ~ must be a
#'                variable of class Surv and the right side of ~ must have
#'                variables in an additive way.
#' @param data  A data.frame contains data for analysis.
#' @param digits An integer indicating the number of decimal places (round) or
#'               significant digits to be used. Default value is 2.
#' @export
#' @return a data.frame consist of hazard ratio and 95\% confidence intervals and
#'         the p values.
#' @examples
#' require(survival)
#' data(colon)
#' attach(colon)
#' colon$TS=Surv(time,status==1)
#' out=mycph(TS~.,data=colon)
#' out
#' HRplot(out,type=2,show.CI=TRUE,main="Hazard ratios of all individual variables")
mycph=function(formula,data,digits=2){
    call=paste(deparse(formula),", ","data= ",substitute(data),sep="")
    cat("\n mycph : perform coxph of individual expecting variables\n")
    cat("\n Call:",call,"\n\n")
    f=formula
    myt=terms(f,data=data)
    y=as.character(f[[2]])
    if(class(data[[y]])!="Surv") {
        cat(y, "is not an object of class Surv")
        return(invisible())
    }
    myvar=attr(myt,"term.labels")
    count=length(myvar)
    var<-HR<-lcl<-ucl<-p.value<-c()
    for(i in 1:count) {
        s=paste(y,myvar[i],sep="~")
        suppressWarnings(out<-summary(survival::coxph(as.formula(s),data)))
        if(any(is.infinite(out$conf.int))){
            cat(dimnames(out$conf.int)[[1]]," was excluded : infinite\n")
            next
        }
        if(any(is.nan(out$coef))){
            cat(dimnames(out$conf.int)[[1]]," was excluded : NaN\n")
            next
        }
        if(any(is.na(out$coef))){
            cat(dimnames(out$conf.int)[[1]]," was excluded : NA\n")
            next
        }
        var=c(var,dimnames(out$conf.int)[[1]])
        HR=c(HR,out$coef[,2])
        lcl=c(lcl,out$conf.int[,3])
        ucl=c(ucl,out$conf.int[,4])
        p.value=c(p.value,out$coef[,5])
    }
    if(length(HR)<1) return(invisible())
    result=round(data.frame(HR,lcl,ucl),digits)
    rownames(result)=var
    result=cbind(result,round(p.value,max(3,digits)))
    colnames(result)[4]="p"
    result
}

#' Extract hazard ratio from a data.frame
#'
#' @param x an object of class coxph
#' @param digits An integer indicating the number of decimal places (round) or
#'               significant digits to be used. Default value is 2.
#' @export
#' @return a data.frame consist of hazard ratio and 95% confidence intervals and
#'         the p values.
#'
extractHR=function(x,digits=2){
    digits=2
    out=summary(x)
    res=data.frame(round(out$conf.int[,-2],digits))
    res=cbind(res,round(out$coef[,5],max(3,digits)))
    colnames(res)=c("HR","lcl","ucl","p")
    res
}

#' Draw a hazard ratio plot
#'
#' @param out an object of class coxph or a resultant data.frame of mycph function
#' @param type an integer indicating the type of plot. Default value is 1
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#' @param show.OR a logical vector indicating whether or not show the text
#'                indicating the p value
#' @param show.CI a logical vector indicating whether or not show the text
#'                indicating the confidence interval
#' @param sig.level a numeric value of upper limit of p value of showing variables
#' @param cex A numerical value giving the amount by which plotting OR/HR symbols
#'            should be magnified relative to the default, defaulting 1.2.
#' @param lwd The line width, a positive number, defaulting to 2.
#' @param pch Either an integer specifying a symbol or a single character
#'           to be used as the default in plotting OR/HR points.
#' @param col A specification for the default plotting color.
#' @param ... arguments to be passed to plot
#' @return This function return NULL invisibly and draw graphs
#' @export
#' @examples
#' require(survival)
#' attach(colon)
#' colon$TS=Surv(time,status==1)
#' out=mycph(TS~.,data=colon)
#' out
#' HRplot(out,type=1,pch=2,col=c("blue","red"))
#' HRplot(out,type=2,show.CI=TRUE,pch=2,cex=2,main="Hazard ratios of all individual variables")
#'
HRplot=function(out,type=1,xlab="",ylab="",show.OR=TRUE,show.CI=FALSE,
                sig.level=1,cex=1.2,lwd=2,pch=18,col=NULL,...){
    if(class(out)=="coxph") res=extractHR(out)
    else res=out
    exclude=unname(which(apply(res,1,function(x) any(is.nan(x))|any(x>10^5))))
    if(length(exclude)>0) res=res[-exclude,]
    ORplot.sub(res,type,xlab,ylab,show.OR,show.CI,sig.level,
               cex=cex,lwd=lwd,pch=pch,col=col,...)
}
