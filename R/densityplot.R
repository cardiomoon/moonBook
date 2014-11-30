#' Make Kernel density plot
#' 
#' @param formula an R model formula, of the form ~ variable to estimate the 
#'     unconditional density of variable, or variable ~ factor to estimate the 
#'     density of variable within each level of factor.
#' @param data an optional data frame containing the data.   
#' @param main main title of plot 
#' @param xlab label for the horizontal-axis; defaults to the name of the 
#'             variable x. 
#' @param ylab label for the vertical axis; defaults to "Density".             
#' @param ... arguments to be passed to plot
#' @return This function return NULL invisibly and draw graphs.
#' @examples
#'   require(moonBook)
#'   data(acs)
#'   densityplot(age~Dx,data=acs)
densityplot=function(formula,data,main="",xlab="",ylab="",...){
        call=paste(deparse(formula),", ","data= ",substitute(data),sep="")
        #cat("\n Call:",call,"\n\n")
        f=formula
        myt=terms(f,data=data)
        y=as.character(f[[3]])
        #cat("Grouping variables :",y, ",class: ",class(data[[y]]),"\n")
        y=unlist(strsplit(y,"+",fixed=TRUE))
        if(length(y)>1) {
            cat("\n","Only one dependent variable is permitted\n")
            return(invisible())
        }
        y1=validColname(y,colnames(data))
        if(is.na(y1)) {
            cat("\n","There is no column named '",y,"' in data ",
                substitute(data),"\n")
            return(invisible())
        }
        if(!identical(y,y1)) {
            cat("\n","'",y,
                "' is an invalid column name: Instead '",y1,"' is used\n")
            s=paste(as.character(f[[2]]),y1,sep="~")
            result=densityplot(as.formula(s),data)
            return(invisible())
        } 
        x=as.character(f[[2]])
        x1=validColname(x,colnames(data))
        if(is.na(x1)) {
            cat("\n","There is no column named '",x,"' in data ",
                substitute(data),"\n")
            return(invisible())
        }
        if(!identical(x,x1)) {
            cat("\n","'",x,
                "' is an invalid column name: Instead '",x1,"' is used\n")
            s=paste(x1,as.character(f[[3]]),sep="~")
            result=densityplot(as.formula(s),data)
            return(invisible())
        } 
        if(is.factor(data[[y1]])) group=data[[y1]]
        else group=factor(data[[y1]])
        count=length(levels(group))
        colors=rainbow(count)
        if(nchar(main)==0) main=paste("Distribution of '",x,"' by '",y1,"'")
        if(nchar(xlab)==0) xlab=x
        if(nchar(ylab)==0) ylab="Density"
        xrange=range(data[[x]],na.rm=TRUE)
        dl=list()
        maxy=0
        for(i in 1:count) {
            subdata=subset(data,data[[y]]==levels(group)[i])
            d=density(subdata[[x]],na.rm=T)
            maxy=max(maxy,max(d$y))
            dl[[i]]=d  
        }
        plot(c(xrange[1]*0.85,xrange[2]*1.1),c(0,maxy*1.1),
             main=main,xlab=xlab,ylab=ylab,type="n",...)
        for(i in 1:count) lines(dl[[i]],col=colors[i],lty=i,lwd=2)
        legend("topright",legend=levels(group),col=colors,lty=1:count)
}

