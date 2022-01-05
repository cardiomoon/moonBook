#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param y a vector
#' @param x a numeric vector
#' @importFrom stats lm shapiro.test resid var.test t.test kruskal.test anova wilcox.test
#' @export
my.t.test=function(y,x){

    result=table(y,x)
    dim(result)

    (xlev=dim(result)[1])
    (ylev=dim(result)[1])
    if(ylev==1) {
        p=c(NA,NA,NA)
    } else if(xlev==2) {
        #browser()
        out=lm(x~y)
        if(sum(result)<=5000) {
            if(length(unique(resid(out)))==1){
               out3=1
            } else{
               out3=shapiro.test(resid(out))
            }

        } else {
            out3=nortest::ad.test(resid(out))
        }
        out1=try(var.test(x~y))

        if(class(out1)!="htest") {
            p=c(NA,NA,NA)
        } else{
            options(warn=-1)
            out5<-wilcox.test(x~y)
            options(warn=0)
            if(is.nan(out1$p.value)) {
                p=c(NA,NA,out5$p.value)
            } else if(out1$p.value<0.05) {
                out4=t.test(x~y,na.rm=T)
                p=c(out3$p.value,out4$p.value,out5$p.value)
            } else {
                out4=t.test(x~y,var.equal=TRUE)
                p=c(out3$p.value,out4$p.value,out5$p.value)
            }

        }

    } else{
        out3=lm(x~factor(y))
        if(sum(result)<=5000) out4=shapiro.test(resid(out3))
        else out4=nortest::ad.test(resid(out3))
        out5=kruskal.test(as.numeric(x),factor(y))
        p=c(out4$p.value,anova(out3)$Pr[1],out5$p.value)
    }
    p
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param x a vector
#' @param y a vector
#' @param mydata A data.frame
#' @param catMethod An integer indicating methods for categorical variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{0}{Perform chisq.test first. If warning present, perform fisher test}
#'                  \item{1}{Perform chisq.test without continuity correction}
#'                  \item{2}{Perform chisq.test with continuity correction}
#'                  \item{3}{perform fisher.test}
#'                  \item{4}{perform prop.trend test}
#'               }
#'               Default value is 2.
#' @importFrom stats chisq.test fisher.test xtabs prop.trend.test
#' @export
my.chisq.test=function(x,y,mydata,catMethod=2)
{
  # x="sex"
  # y="Dx"
  # mydata=acs[c(x,y)]
  # str(mydata)
  # colnames(mydata)=c("x","y")
  #  catMethod=2
    temp=table(mydata$y,mydata$x)
    # temp
    # str(x)
    #  str(y)
    #  str(mydata)
    if((nrow(temp)>2)&(ncol(temp)==2)) temp=t(temp)
    # temp=xtabs(~x+y)
     temp
    if(dim(temp)[2]==1){
        p=c(NA,NA,NA)
        attr(p,"method")=""
    # } else if(dim(temp)[1]==1){
    #     p=c(NA,NA,NA)
    #     attr(p,"method")=""
    } else{
        p=c(NA,NA,NA)
        ow=options("warn")
        options(warn=-1)
        if(catMethod==0) {
          result=cat.test(temp)
        } else if(catMethod==1) {
          result=chisq.test(temp,correct=FALSE)
        } else if(catMethod==2) {
          result=chisq.test(temp)
        } else if(catMethod==3) {
          result=cat.test(temp,mode=2)
        } else if(catMethod==4) {
            if(nrow(temp)>2) {
                result=NA
            } else {
                result=prop.trend.test(temp[2,],colSums(temp))
            }
        }

        if(length(result)==1){
            p[1]<-NA
            attr(p,"method")=""
        } else{
           p[1]=result$p.value
           attr(p,"method")=result$method
        }
        if(sum(temp)< 100 & dim(temp)[1]>1){
            p[2]=fisher.test(temp)$p.value
        }
        if(nrow(temp)!=2) {
            p[3]=NA
        } else {
            p[3]=prop.trend.test(temp[2,],colSums(temp))$p.value
        }
        options(ow)
    }
     if(is.nan(p[1])) p[1]=1
    p
}

#' Perform chisq.test or fisher test
#' @param x a numeric vector or matrix. x and y can also both be factors.
#' @param mode An integer. If 1(default), perform chisq.test first, If 2, perform fisher.test first
#' @param ... Further arguments to be passed to chisq.test or fisher.test
#' @export
cat.test=function(x,mode=1,...){

  result=tryCatch(chisq.test(x,...),warning=function(w) return("warning present"))
  if((mode==1) & ("htest" %in% class(result))) return(result)

  result2=tryCatch(fisher.test(x,...),
                     warning=function(w) return("warning present"),
                     error=function(e) return("error present"))
  if("htest" %in% class(result2)) result=result2
  result
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param x a numeric vector
#' @importFrom stats sd median mad IQR fivenum
#' @export
num_summary <-function(x){
    if(all(is.na(x))){
       result=list(NA,NA,NA,NA,NA,list(NA,NA,NA,NA,NA))
    } else{
      funs=c(mean,sd,median,mad,IQR,fivenum)
      result=lapply(funs,function(f) f(x,na.rm=TRUE))
    }
    result

}


#' Produce table for descriptive statistics
#'
#' Produce table for descriptive statistics by groups for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA,Kruskal-Wallis, chisq, Fisher,...)
#' @param x An R object, formula or data.frame
#' @param ... arguments to be passed to \code{\link{mytable_sub}}
#' @export
#' @examples
#' mytable(acs)
#' mytable(~age+sex,data=acs)
#' mytable(Dx~age+sex+height+weight+TC+TG+HDLC,data=acs,method=3,digits=2)
#' mytable(am+cyl~.,data=mtcars)
#' out=mytable(sex~.,data=acs)
#' out
#' summary(out)
#' \dontrun{
#' require(ztable)
#' ztable(out)
#' }
mytable=function(x,...)  UseMethod("mytable")


#'@describeIn mytable S3 method for formula
#'@export
mytable.formula=function(x,...) {
    mytable_sub(x,...)
}


#' Produce table for descriptive statistics
#'
#' Produce table for descriptive statistics by groups for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA,Kruskal-Wallis, chisq, Fisher,...)
#'
#' @param x An object of class "formula". Left side of ~ must contain the
#'                name of one grouping variable or two grouping variables in an
#'                additive way(e.g. sex+group~), and the right side of ~ must have
#'                variables in an additive way.
#' @param data A data.frame contains data for analysis
#' @param use.labels Logical. Whether or not use labels.
#' @param use.column.label Logical. Whether or not use column labels.
#' @param max.ylev An integer indicating the maximum number of levels of grouping
#'                 variable ('y'). If a column have unique values less than max.ylev
#'                 it is treated as a categorical variable. Default value is 5.
#' @param maxCatLevel An integer indicating the maximum number of unique levels of categorical variable.
#'                  If a column have unique values more than maxCatLevel, categorical summarization
#'                  will not be performed.
#' @param digits An integer indicating the number of decimal places (round) or
#'               significant digits to be used. Default value is 1.
#' @param method An integer indicating methods for continuous variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{1}{forces analysis as normal-distributed}
#'                  \item{2}{forces analysis as continuous non-normal}
#'                  \item{3}{performs a Shapiro-Wilk test to decide between
#'                          normal or non-normal}
#'               }
#'               Default value is 1.
#' @param catMethod An integer indicating methods for categorical variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{0}{Perform chisq.test first. If warning present, perform fisher test}
#'                  \item{1}{Perform chisq.test without continuity correction}
#'                  \item{2}{Perform chisq.test with continuity correction}
#'                  \item{3}{perform fisher.test}
#'                  \item{4}{perform prop.trend test}
#'               }
#'               Default value is 2.
#' @param show.all A logical value indicating whether or not all statistical
#'                 values have to be shown in table. Default value is FALSE.
#' @param exact A logical value indicating whether or not permit call with approximate
#'             parameter. If true, only exact column name permitted.Default value is FALSE.
#' @param show.total A logical value indicating whether or not show total group value.
#'                 Default value is FALSE.
#' @param missing A logical value indicating whether or not perform missing data analysis.
#'                 Default value is FALSE.
#' @return An object of class "mytable".
#'      'print' returns a table for descriptive statistics.
#'      'summary' returns a table with all statistical values.
#'
#' @importFrom stats addmargins
#' @export
mytable_sub=function(x,data,use.labels=TRUE,use.column.label=TRUE,
                     max.ylev=5,maxCatLevel=20,digits=1,method=1,catMethod=2,
                     show.all=FALSE,exact=FALSE,show.total=FALSE,missing=FALSE){
    # x=Sex~.
    # data=acs;use.labels=TRUE;use.column.label=TRUE
    # max.ylev=5;maxCatLevel=20;digits=1;method=1;show.all=FALSE;exact=FALSE;show.total=FALSE

    call=paste(deparse(x),", ","data= ",substitute(data),sep="")
    # cat("\n Call:",call,"\n\n")
    f=x
    length(f)
    myt=terms(f,data=data)

    if(length(f)>2) {
        y=as.character(f[[2]])
    } else{
        y=""
    }
    y
    res=unlist(strsplit(deparse(x),"~",fixed=TRUE))
    # if(y!="") y=unlist(strsplit(y,"+",fixed=TRUE))
    if(length(y)>1) {
        result=mytable2(x,data,use.labels,use.column.label,
                        max.ylev,maxCatLevel,digits,method=method,catMethod=catMethod,show.all,exact=exact,show.total=show.total)
        return(result)
    }
    if(y!=""){
    if(exact){
        y1<-y
    } else{
        y1=validColname(y,colnames(data))
        if(is.na(y1)) {
            cat("\n","There is no column named '",y,"' in data ",substitute(data),"\n")
            return(invisible())
        }
        if(!identical(y,y1)) {
            cat("\n","'",y,"' is an invalid column name: Instead '",y1,"' is used\n")
            s=paste(y1,res[2],sep="~")

            result=mytable(as.formula(s),data,use.labels,use.column.label,
                           max.ylev,maxCatLevel,digits,method,show.all,exact=exact,show.total=show.total)

            attr(result,"missing")=TRUE
            return(result)
        }
    }
    if(missing==TRUE){
        if(sum(is.na(data[[y]]))>0){
        data[[paste0(y,"Missing")]]=ifelse(is.na(data[[y]]),"Missing","Not missing")
        data[[paste0(y,"Missing")]]=factor(data[[paste0(y,"Missing")]],levels=c("Not missing","Missing"))
        s=paste0(paste0(y,"Missing"),"~",res[2])
        data[[y]]<-NULL

        result=mytable(as.formula(s),data,use.labels,use.column.label,
                       max.ylev,maxCatLevel,digits,method,show.all,exact=exact,show.total=show.total)
        attr(result,"missing")=TRUE
        return(result)
        } else{
            cat(paste0("There is no missing data in column '",y,"'\n"))
            s=paste0("~",res[2])
            result=mytable(as.formula(s),data,use.labels,use.column.label,
                           max.ylev,maxCatLevel,digits,method,show.all,exact=exact,show.total=show.total)
            return(result)
        }
    }
    # if(use.column.label){
    #     data<-changeColnameLabel(data,f)
    # }
    if(use.labels){
            data<-addLabelDf(data)
    }
    t=table(data[[y1]])
    if(show.total){
        t=addmargins(t)
        names(t)[length(t)]="Total"
    }
    labely1=y1
    if(use.column.label){

        label=attr(data[[y1]],"label",exact=TRUE)
        if(!is.null(label)) labely1=label


    }
    result=list(y=labely1,length=length(t),names=names(t),count=unname(t),method=method,show.all=show.all)

    x=labels(myt)
    error=c()



    for(i in 1:length(x)) {

        out<-mytable_sub2(y1,x[i],data,max.ylev,maxCatLevel,method=method,catMethod=catMethod,show.total=show.total,origData=data)

        if(length(out)!=4) {
            error=c(error,x[i])
            next
        }
        label=getLabel(data,x[i],use.column.label)
        result[[label]]=out
    }
    #str(result)
    out=printmytable2(result,digits)
    class(out)=c("mytable")
    attr(out,"error")=error
    out
    } else{
        dataname=as.character(substitute(data))
        # str(dataname)
        assign(dataname,data[attr(myt,"term.labels")])
        out=eval(parse(text=paste0("mytable_df(",dataname,
                                   ",max.ylev=",max.ylev,",digits=",digits,",method=",method,
                                   ",show.all=",show.all,")")))
        out
    }
    out
}


#' Find valid string among character vector from approximate string
#'
#' @param pattern character string to be matched in the given character
#' @param x a character vector where matches are sought
#'
#' @return returns NA in case of no matched string found
#'         or a character string in string vector x
#' @export
#' @examples
#' a="dx"
#' b=c("Age","Sex","Dx")
#' validColname(a,b)
#'
validColname=function(pattern,x) {
    result=which(grepl(pattern,x,ignore.case=TRUE))
    if(length(result)<1) return(NA)
    else if(length(result)>1) result=result[1]
    x[result]
}


#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param y a vector
#' @param x a vector
#' @param data a data.frame
#' @param max.ylev an integer
#' @param maxCatLevel an integer
#' @param method an integer
#' @param catMethod an integer
#' @param show.total a logical value
#' @param origData a data.frame
#' @importFrom stats na.omit
#' @export
mytable_sub2=function(y,x,data,max.ylev=5,maxCatLevel=20,method=1,catMethod=2,show.total=FALSE,origData){
    #mydata=na.omit(data.frame(y=data[[y]],x=data[[x]]))
    # data=iris2
    # y="Species"
    # x="Sepal.Length"
     # use.column.label=TRUE;max.ylev=5;maxCatLevel=20;method=1;show.total=FALSE
    mydata=try(data.frame(y=data[[y]],x=data[[x]]))

    if(class(mydata)!="data.frame") return(-1)
    result=table(mydata$x,mydata$y)

    result1=addmargins(result,2)
    N=sum(result)
    var_name=x
    # xlev=dim(result)[1]
    xlev=length(setdiff(unique(origData[[x]]),NA))
    # cat("xlev=",xlev,"\n")
    # str(data)
    # str(origData)

    var_class=ifelse(is.numeric(mydata$x),"continuous","categorical")
    if(xlev<=max.ylev) {
        factorx=factor(mydata$x)
        var_class="categorical"
    }
     # cat("name=",var_name,",class=",var_class,"N=",N,"\n")
    if(var_class=="categorical") {   # categorical
        subgroup=list()
        ## for descriptives
        if(xlev<=maxCatLevel){
        for(i in 1:xlev){
            if(show.total){

                count=result1[i,]
                attr(count,"names")=NULL
                ratio=apply(result1,2,function(x) x*100/sum(x))
                #result2=cbind(result1[,ncol(result1)],result1[,-ncol(result1)])
                #count=result2[i,]
                #attr(count,"names")=NULL
                #ratio=apply(result2,2,function(x) x*100/sum(x))
                if(!is.null(dim(ratio))) a=list(count=count,ratio=ratio[i,])
                else a=list(count=count,ratio=ratio)
                subgroup[[i]]=a

            }else{
                count=result[i,]
                attr(count,"names")=NULL
                ratio=apply(result,2,function(x) x*100/sum(x))
                #browser()
                if(!is.null(dim(ratio))) a=list(count=count,ratio=ratio[i,])
                else a=list(count=count,ratio=ratio)
                subgroup[[i]]=a
            }

        }
        #if(xlev==1) {
        #    for(i in 2:length(unique(data[[x]]))) {
        #        ratio=count=rep(0,length(result))
        #        a=list(count=count,ratio=ratio)
        #        #subgroup[[i]]=a
        #    }
        #}
        names(subgroup)=rownames(result)
        ## for statistical
        p=my.chisq.test(x,y,mydata,catMethod=catMethod)
        } else{
            var_class="categorical2"
            if("Date" %in% class(mydata$x)){
                subgroup=paste0(min(mydata$x),"-",max(mydata$x))
            } else{
               subgroup=paste0("unique values:",xlev)
            #    cat("xlev=",xlev,"\n")
            #    str(data)
            #    str(origData)
            }
            p=NA
        }
        result=list(class=var_class,count=N,subgroup=subgroup,p=p)
        #str(result)
        result
        #browser()
    } else {
         ## for descriptive
         out=tapply(data[[x]],data[[y]],num_summary)

         if(show.total){
            out[[length(out)+1]]=num_summary(data[[x]])
            names(out)[length(out)]="Total"
        }
        if(FALSE){
        out=list()
        if(show.total){
            out=num_summary(data[[x]])
            names(out)[length(out)]="Total"
            out1=tapply(data[[x]],data[[y]],num_summary)
            for(i in 1:length(out1)){
                out[[i+1]]=out1[[i]]
            }
        }
        else{
            out=tapply(data[[x]],data[[y]],num_summary)
        }
        }
        # for statistics
        mydata=na.omit(data.frame(y=data[[y]],x=data[[x]]))
        p=my.t.test(mydata$y,mydata$x)
        result=list(class=var_class,count=N,out=out,p=p)

    }
    # str(result)
    result
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param obj an object
#' @param digits an integer
#' @export
printmytable2=function(obj,digits=1){
    # obj<-result;digits=1
    # str(obj)
    plusminus="\u00b1"
    cl=c()
    N=c()
    varnames=c()
    subnames=c()
    p1=p2=p3=p4=c()
    ptest=c()
    desc=matrix(,ncol=obj$length)

    colnames(desc)=obj$names

    fmt=sprintf("%s%df","%4.",digits)
    fmt
    #str(obj)
    for(i in 7:length(obj)){
        varnames=c(varnames,names(obj)[i])
        subnames=c(subnames,"")
        cl=c(cl,obj[[i]][1])
        N=c(N,obj[[i]][2])
        add=matrix(,ncol=obj$length)

        # if numeric
        if(obj[[i]]$class=="continuous"){
            for(j in 1:obj$length){
                if(is.na(obj[[i]]$out[[j]][[1]])) temp1="    -"
                else temp1=paste(sprintf(fmt,obj[[i]]$out[[j]][[1]]),plusminus,
                            sprintf(fmt,obj[[i]]$out[[j]][[2]]),sep=" ")
                if(is.na(obj[[i]]$out[[j]][[6]][3])) temp2="    -"
                else temp2=paste(sprintf(fmt,obj[[i]]$out[[j]][[6]][3])," [",
                            sprintf(fmt,obj[[i]]$out[[j]][[6]][2]),";",
                            sprintf(fmt,obj[[i]]$out[[j]][[6]][4]),"]",sep="")
                if(obj$method==1) temp=temp1
                else if(obj$method==2) temp=temp2
                else if(obj$method==3) {
                    if(is.na(obj[[i]]$p[1])){
                        temp=temp2
                    } else if(obj[[i]]$p[1]<=0.05) {
                        temp=temp2
                    } else {
                        temp=temp1
                    }
                }
                add[1,j]=temp

            }
            if(all(is.na(desc))) desc=add
            else desc=rbind(desc,add)
            p1=c(p1,obj[[i]]$p[1])
            p2=c(p2,obj[[i]]$p[2])
            p3=c(p3,obj[[i]]$p[3])
            if(obj$method==1) {
                p4=c(p4,obj[[i]]$p[2])
                ptest=c(ptest,"normal")
            }
            else if(obj$method==2) {
                p4=c(p4,obj[[i]]$p[3])
                ptest=c(ptest,"non-normal")
            }
            else if(obj$method==3) {
                if(is.na(obj[[i]]$p[1])) {
                    p4=c(p4,obj[[i]]$p[3])
                    ptest=c(ptest,"non-normal")
                } else if(obj[[i]]$p[1]<=0.05) {
                    p4=c(p4,obj[[i]]$p[3])
                    ptest=c(ptest,"non-normal")
                } else{
                    p4=c(p4,obj[[i]]$p[2])
                    ptest=c(ptest,"normal")
                }
            }
        }
        # if factor
        else if(obj[[i]]$class=="categorical"){          ##if(obj[[i]]$class=="categorical")
            add=matrix(,ncol=obj$length)
            for(j in 1:obj$length){
                   add[1,j]=""
            }
            if(all(is.na(desc))) desc=add
            else desc=rbind(desc,add)

            p1=c(p1,obj[[i]]$p[1])
            p2=c(p2,obj[[i]]$p[2])
            p3=c(p3,length(obj[[i]]$subgroup))
            #p3=c(p3,NA)
            p4=c(p4,obj[[i]]$p[1])
            ptest=c(ptest,attr(obj[[i]]$p,"method"))
            for(k in 1:length(obj[[i]]$subgroup)){
                temp=names(obj[[i]]$subgroup)[k]
                varnames=c(varnames,"")
                subnames=c(subnames,temp)
                cl=c(cl,"")
                N=c(N,"")
                p1=c(p1,NA)
                p2=c(p2,NA)
                p3=c(p3,NA)
                p4=c(p4,NA)
                ptest=c(ptest,"")
                add=matrix(,ncol=obj$length)
                for(j in 1:obj$length){
                    if(is.na(obj[[i]]$subgroup[[k]]$count[j])) temp="   -"
                    else if(obj[[i]]$subgroup[[k]]$count[j]==0) temp=" 0 ( 0.0%)"
                    else temp=paste(obj[[i]]$subgroup[[k]]$count[j]," (",
                               sprintf(fmt,obj[[i]]$subgroup[[k]]$ratio[j]),"%)",sep="")
                    add[1,j]=temp
                }
                if(all(is.na(desc))) desc=add
                else desc=rbind(desc,add)
            }
        }
        else{   ##if(obj[[i]]$class=="categorical2")
            for(j in 1:obj$length){
                add[1,j]=obj[[i]]$subgroup
            }
            if(all(is.na(desc))) desc=add
            else desc=rbind(desc,add)
            p1=c(p1,NA)
            p2=c(p2,NA)
            p3=c(p3,NA)
            p4=c(p4,NA)
            ptest=c(ptest,"")
        }
    }
    nname=ifelse(subnames=="",varnames,paste(varnames,"  - ",subnames,sep=""))
    nname=formatC(nname,"%s",flag="-")
    options(stringsAsFactors=FALSE)
    res=data.frame(name=nname)
    for(j in 1:obj$length){
        res=data.frame(res,desc[,j])
        #colnames(res)[length(res)]=obj$names[j]
    }
    sp1=sapply(p1,function(x) ifelse(is.na(x),"",sprintf("%.3f",x)))
    sp2=sapply(p2,function(x) ifelse(is.na(x),"",sprintf("%.3f",x)))
    sp3=sapply(p3,function(x) ifelse(is.na(x),"",sprintf("%.3f",x)))
    sp4=sapply(p4,function(x) ifelse(is.na(x),"",sprintf("%.3f",x)))
    sig=sapply(p4,p2sig)
    # str(res)
    # str(ptest)
    res=data.frame(res,p=sp4,sig,p1=sp1,p2=sp2,p3=sp3,class=unlist(cl),ptest=ptest,N=unlist(N))
    #rownames(res)=names(obj)[4:length(obj)]
    colnames(res)[2:(2+length(obj$names)-1)]=obj$names
    if(length(obj$y)==1) colnames(res)[1]=obj$y
    result=list(res=res,count=obj$count,method=obj$method,show.all=obj$show.all)
    result
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param value a numeric vector
p2sig=function(value){
    if(is.na(value)) sig="   "
    else if(value<0.01) sig="***"
    else if(value<0.05) sig="** "
    else if(value<0.1) sig="*  "
    else sig="   "
    sig
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param x a character vector
#' @param ...  further arguments passed to or from other methods.
#' @param width an integer
#' @export
centerprint=function(x,...,width=10){

        mwidth=max(nchar(x),width)
        sp=(mwidth-nchar(x))/2
        front=end=""
        front=space(ceiling(sp))
        end=space(floor(sp))
        x=paste(front,x,end,sep="")
        x
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param num an integer
space=function(num){
    ret=c()
    if(num <1) return(ret)
    for(i in 1:num) ret=paste(" ",ret,sep="")
    ret
}

#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param x a character vector
#' @param times an integer
#' @export
reprint=function(x,times){
    ret=x
    if(times<=1) return(x)
    for(i in 1:times) ret=paste(ret,x,sep="")
    ret
}
#' Internal mytable functions
#'
#' Internal mytable functions
#' These are not to be called by the user
#' @param myobj an R object
obj2linecount=function(myobj){

    if(myobj$show.all==TRUE) out1=myobj$res
    else out1=myobj$res[1:(length(myobj$res)-7)]

    count=myobj$count
    cn=colnames(out1)
    y=cn[1]
    cn[1]=""
    ncount=c("",paste("(N=",count,")",sep=""))
    cn.nchar=nchar(cn)
    col.nchar=unname(sapply(out1,function(x) max(nchar(x))))
    col.length=apply(rbind(cn.nchar,col.nchar),2,max)
    linelength=sum(col.length)+length(cn)-1
    result=list(y=y,out1=out1,cn=cn,ncount=ncount,
                col.length=col.length,linelength=linelength)
    result
}


#' Print function for class "mytable"
#'
#' @param x An object of class "mytable", a result of a call to \code{\link{mytable}}
#' @param ... further arguments passed to or from other methods.
#' @importFrom crayon red
#' @export
print.mytable=function(x,...) {

    myobj=x
    result=obj2linecount(myobj)
    y=result$y
    out1=result$out1
    cn=result$cn
    ncount=result$ncount
    col.length=result$col.length
    linelength=result$linelength

    cat("\n")
    if(!is.null(attr(x,"missing"))){
        cat(centerprint(paste0("Missing data analysis: '",sub("Missing","",y),"'"),
                        width=linelength))
    } else{
    cat(centerprint(paste("Descriptive Statistics by '",y,"'",sep=""),
                    width=linelength))
    }
    cat("\n")
    hline=reprint("\u2014",linelength)  #head line
    tline=reprint("\u2014",linelength)  # tail line
    cat(hline,"\n")
    for(j in 1:(length(cn))) {
        cat(centerprint(cn[j],width=col.length[j]+1))
    }
    cat("\n")
    for(j in 1:length(ncount)) {
        cat(centerprint(ncount[j],width=col.length[j]+1))
    }
    cat("\n")
    cat(tline,"\n")


    for(i in 1:dim(out1)[1]){
        for(j in 1:(length(cn))) {
            if(is.na(as.numeric(out1[i,"p"]))){
                cat(sapply(out1[i,j],centerprint,width=col.length[j]+1))
            } else if(as.numeric(out1[i,"p"])<0.05){
                cat(red(sapply(out1[i,j],centerprint,width=col.length[j]+1)))
            } else{
            cat(sapply(out1[i,j],centerprint,width=col.length[j]+1))
            }

        }
        cat("\n")
    }
    cat(tline,"\n")
}

#' cbind function for class "mytable"
#'
#' @param ...      Objects of class "mytable", a result of a call to \code{\link{mytable}}
#' @param caption  Unique values of grouping variables used for column name of table
#' @param y        Names of grouping variables used for caption of table
#'
#' @export
cbind.mytable=function (..., caption,y=NULL)
{
    cl <- match.call()
    list.names <- function(...) {
        deparse.level <- 1
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm))
            seq_along(l)
        else nm == ""
        dep <- sapply(l[fixup], function(x) switch(deparse.level +
                                                       1, "", if (is.symbol(x)) as.character(x) else "",
                                                   deparse(x, nlines = 1)[1L]))
        if (is.null(nm))
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    args <- list(...)
    cl.miss <- sapply(args, function(args.i) inherits(args.i,
                                                      "missingTable"))
    if (mean(cl.miss) > 0 & mean(cl.miss) < 1)
        stop("All or none of the tables must be of class 'missingTable'")
    if (missing(caption))
        caption <- list.names(...)
    else {
        #if (!is.null(caption))
            #if (length(caption) != length(args))
                #stop("length of caption must be the number of 'mytable' objects to be combined")
    }
    cc <- unlist(lapply(args, function(x) !class(x)[1] %in% c("mytable")))
    #if (any(cc))
    #    stop("arguments must be of class 'mytable' and cannot be of class 'cbind.mytable'")
    out <- args
    if (is.null(caption) || all(caption == ""))
        caption = unlist(lapply(args, function(vv) ifelse(is.null(attr(vv,
                                                                       "yname")), "[No groups]", paste("By", attr(vv, "yname")))))
    attr(out, "caption") <- caption
    attr(out,"group")<-y
    class(out) <- c("cbind.mytable", class(args[[1]]))
    out
}

#' Print function for class "cbind.mytable"
#'
#' @param x   an object of class "cbind.mytable", a result of a call to \code{\link{cbind.mytable}}
#' @param ...  further arguments passed to or from other methods.
#'
#' @export
print.cbind.mytable=function(x,...) {
    myobj=x
    tcount=length(myobj) # number of tables
    tnames=unlist(attr(myobj,"caption"))
    group=attr(myobj,"group")
    result=list()

    for(i in 1:tcount) result[[i]]=obj2linecount(myobj[[i]])

    linelength=0
    for(i in 1:tcount) linelength=linelength+result[[i]]$linelength-result[[i]]$col.length[1]-1
    linelength=result[[1]]$col.length[1]+linelength+tcount

    cat("\n")
    temp=paste("Descriptive Statistics stratified by '",group[1],"' and '",group[2],"'",sep="")
    #for(i in 2:tcount) temp=paste(temp," and '",group[i],"'",sep="")
    cat(centerprint(temp,width=linelength))
    cat("\n")
    hline=reprint("\u2014",linelength)  #head line
    tline=reprint("\u2014",linelength)  # tail line
    cat(hline,"\n")
    cat(centerprint("",width=result[[1]]$col.length[1]+2))
    for(i in 1:tcount){
        if(class(tnames[i])=="factor") temp=levels(tnames)[tnames[i]]
        else temp=tnames[i]
        #browser()
        cat(centerprint(temp,width=result[[i]]$linelength-result[[i]]$col.length[1]+1))
    }
    cat("\n")
    cat(centerprint("",width=result[[1]]$col.length[1]+2))
    for(i in 1:tcount) cat(reprint("\u2014",result[[i]]$linelength-result[[i]]$col.length[1]-2),"")
    cat("\n")
    cat(centerprint(result[[1]]$cn[1],width=result[[1]]$col.length[1]+1))
    for(i in 1:tcount) {
        for(j in 2:(length(result[[i]]$cn))) {
            cat(centerprint(result[[i]]$cn[j],width=result[[i]]$col.length[j]+1))
        }

    }
    cat("\n")
    cat(centerprint("",width=result[[1]]$col.length[1]+1))
    for(i in 1:tcount) {
        for(j in 2:(length(result[[i]]$ncount))) {
            cat(centerprint(result[[i]]$ncount[j],
                            width=result[[i]]$col.length[j]+1))
        }
        cat("     ")
    }
    cat("\n")
    cat(tline,"\n")

    for(i in 1:dim(result[[1]]$out1)[1]){
        for(k in 1:tcount){
            if(k==1) {
                for(j in 1:(length(result[[1]]$cn))){
                    temp=as.numeric(result[[k]]$out1[i,"p"])
                    if(!is.na(temp) &(temp<0.05)){
                        cat(red(sapply(result[[k]]$out1[i,j],centerprint,
                                   width=result[[k]]$col.length[j]+1)))
                    } else {
                    cat(sapply(result[[k]]$out1[i,j],centerprint,
                               width=result[[k]]$col.length[j]+1))
                    }
                }
            }
            else {
                for(j in 2:(length(result[[1]]$cn))){
                    temp=as.numeric(result[[k]]$out1[i,"p"])
                    if(!is.na(temp) &(temp<0.05)){
                       cat(red(sapply(result[[k]]$out1[i,j],centerprint,
                                   width=result[[k]]$col.length[j]+1)))
                    } else{
                    cat(sapply(result[[k]]$out1[i,j],centerprint,
                               width=result[[k]]$col.length[j]+1))
                    }
                }
            }
        }
        cat("\n")
    }


    cat(tline,"\n")
}

#' Summarizing function for class "mytable"
#'
#' @param object An object of class "mytable", a result of a call \code{\link{mytable}}
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @examples
#' out=mytable(am~.,data=mtcars)
#' summary(out)
#'
summary.mytable=function(object,...) {
    object$show.all=TRUE
    object
}

#' Summarizing function for class "cbind.mytable"
#'
#' @param object An object of class "cbind.mytable", a result of a call \code{\link{mytable}}
#' @param ... further arguments passed to or from other methods.
#' @export
#' @examples
#' out=mytable(am+cyl~.,data=mtcars)
#' summary(out)
summary.cbind.mytable=function(object,...) {
    tcount=length(object)
    for(i in 1:tcount) {
        object[[i]]$show.all=TRUE
    }
    object
}

#' Produce combined table for descriptive statistics
#'
#' Produce table for descriptive statistics by two grouping variables for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA,Kruskal-Wallis, chisq, Fisher,...)
#'
#' @param formula An object of class "formula". Left side of ~ must contain two grouping variables in an
#'                additive way(e.g. sex+group~), and the right side of ~ must have
#'                variables in an additive way.
#' @param data A data.frame contains data for analysis
#' @param use.labels Logical. Whether or not use labels.
#' @param use.column.label Logical. Whether or not use column labels.
#' @param max.ylev An integer indicating the maximum number of levels of grouping
#'                 variable ('y'). If a column have unique values less than max.ylev
#'                 it is treated as a categorical variable. Default value is 5.
#' @param maxCatLevel An integer indicating the maximum number of unique levels of categorical variable.
#'                  If a column have unique values more than maxCatLevel, categorical summarization
#'                  will not be performed.
#' @param digits An integer indicating the number of decimal places (round) or
#'               significant digits to be used. Default value is 1.
#' @param method An integer indicating methods for continuous variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{1}{forces analysis as normal-distributed}
#'                  \item{2}{forces analysis as continuous non-normal}
#'                  \item{3}{performs a Shapiro-Wilk test to decide between
#'                          normal or non-normal}
#'               }
#'               Default value is 1.
#' @param catMethod An integer indicating methods for categorical variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{0}{Perform chisq.test first. If warning present, perform fisher test}
#'                  \item{1}{Perform chisq.test without continuity correction}
#'                  \item{2}{Perform chisq.test with continuity correction}
#'                  \item{3}{perform fisher.test}
#'                  \item{4}{perform prop.trend test}
#'               }
#'               Default value is 2.
#' @param show.all A logical value indicating whether or not all statistical
#'                 values have to be shown in table. Default value is FALSE.
#' @param exact A logical value indicating whether or not permit call with approximate
#'             parameter. If true, only exact column name permitted.Default value is FALSE.
#' @param show.total A logical value indicating whether or not show total group value.
#'                 Default value is FALSE.
#' @param origData A data.frame contains data for analysis
#' @export
#' @return An object of class "cbind.mytable"
mytable2=function(formula,data,use.labels=TRUE,use.column.label=TRUE,
                  max.ylev=5,maxCatLevel=20,digits=2,method=1,catMethod=2,
                  show.all=FALSE,exact=FALSE,show.total=FALSE,origData=NULL){

    if(is.null(origData)) origData=data
    call=paste(deparse(formula),", ","data= ",substitute(data),sep="")
    # cat("\n Call:",call,"\n\n")
    f=formula
    myt=terms(f,data=data)
    y=as.character(f[[2]])
    res=unlist(strsplit(deparse(formula),"~",fixed=TRUE))
    recall=0
    final=NA
    #cat("Grouping variables :",y, ",class: ",class(data[[y]]),"\n")
    y=unlist(strsplit(y,"+",fixed=TRUE))
    if(length(y)>1) y=y[-1]
    if(length(y)>2) {
        cat("Only two variables are permitted as grouping variables\n")
        return(invisible())
    }
    if(exact) {
        validy1=y[1]
        validy2=y[2]
    } else{
        validy1=validColname(y[1],colnames(data))
        validy2=validColname(y[2],colnames(data))
        if(is.na(validy1)) {
            cat("\n","There is no column named '",y[1],"' in data ","\n")
            return(invisible())
        }
        if(!identical(y[1],validy1)) {
            cat("\n","'",y[1],
                "' is an invalid column name: Instead '",validy1,"' is used\n")
            recall=1
        }
        if(is.na(validy2)) {
            cat("\n","There is no column named '",y[2],"' in data ","\n")
            return(invisible())
        }
        if(!identical(y[2],validy2)) {
            cat("\n","'",y[2],
                "' is an invalid column name: Instead '",validy2,"' is used\n")
            recall=1
        }
        if(recall==1) {
            s=paste(validy1,validy2,sep="+")
            s=paste(s,res[2],sep="~")
            result=mytable2(as.formula(s),data,use.labels,use.column.label,
                            max.ylev,maxCatLevel,digits,method=method,catMethod=catMethod,show.all,exact,show.total,origData=origData)
            return(result)
        }
    }

    # if(use.column.label){
    #     data<-changeColnameLabel(data)
    # }
    if(use.labels){
        data<-addLabelDf(data)
    }
    uniquey=unique(data[[validy1]])
    ycount=length(uniquey)
    out1=list()
    for(i in 1:ycount){
        mydata=data[data[[validy1]]==uniquey[i],]

        if(FALSE){  #mytable
            t=table(data[[y1]])
            if(show.total){
                t=addmargins(t)
                names(t)[length(t)]="Total"
            }
            result=list(y=y1,length=length(t),names=names(t),count=unname(t),
                        method=method,show.all=show.all)
            x=labels(myt)
            for(i in 1:length(x)) {
                out=mytable_sub2(y1,x[i],data,max.ylev,maxCatLevel,method=method,catMethod=catMethod,show.total=show.total,origData=origData)
                label=getLabel(data,x[i],use.column.label)
                result[[label]]=out

            }
            out=printmytable2(result,digits)
            class(out)=c("mytable")
            out

        }

        t=table(mydata[[validy2]])
        if(show.total){
            t=addmargins(t)
            names(t)[length(t)]="Total"
        }
        result=list(y=validy2,length=length(t),names=names(t),count=unname(t),
                    method=method,show.all=show.all)
        x=labels(myt)

        for(j in 1:length(x)) {
            if((length(unique(origData[[x[j]]]))<=max.ylev) & (!is.factor(mydata[[x[j]]]))){
                #cat("x[j]=",x[j],"\n")
                data[[x[j]]]=factor(data[[x[j]]])
                mydata=data[data[[validy1]]==uniquey[i],]
            }
            out=mytable_sub2(validy2,x[j],mydata,max.ylev,maxCatLevel,method=method,catMethod=catMethod,show.total=show.total,origData=origData)

            label=getLabel(data,x[j],use.column.label)
            result[[label]]=out

            #cat("y[2]=",y[2],",x[j]=",x[j],"\n")
        }
        out=printmytable2(result,digits)
        class(out)=c("mytable")
        out1[[i]]=out

    }
    if(use.column.label) {

    }
    y=getLabel(data,y,use.column.label)
    if(ycount==2) final=cbind(out1[[1]],out1[[2]],caption=uniquey,y=y)
    else if(ycount==3) final=cbind(out1[[1]],out1[[2]],out1[[3]],caption=uniquey,y=y)
    else if(ycount==4) final=cbind(out1[[1]],out1[[2]],out1[[3]],out1[[4]],caption=uniquey,y=y)
    else if(ycount==5) final=cbind(out1[[1]],out1[[2]],out1[[3]],out1[[4]],out1[[5]],
                              caption=uniquey,y=y)
    else if(ycount==6) final=cbind(out1[[1]],out1[[2]],out1[[3]],out1[[4]],out1[[5]],
                              out1[[6]],caption=uniquey,y=y)
    else {cat("maximum possible ylevel is six"); return(invisible())}
    final
}


#' rank a numeric vector and returns a new ordinal vector
#'
#' @param y a numeric vector
#' @param k a integer specifies how many groups you want to classify.
#'            default value is 4
#'
#' @return a ordinal vector(numeric) with the same length of y
#'
#' @export
#' @examples
#'
#' require(ggplot2)
#' data(diamonds)
#' diamonds$PriceGroup=rank2group(diamonds$price,4)
#' table(diamonds$PriceGroup)
#' aggregate(price~PriceGroup,data=diamonds,range)
#'
#' diamonds$PriceGroup3=rank2group(diamonds$price,3)
#' table(diamonds$PriceGroup3)
#' aggregate(price~PriceGroup3,data=diamonds,range)

#' diamonds$PriceGroup5=rank2group(diamonds$price,5)
#' table(diamonds$PriceGroup5)
#' aggregate(price~PriceGroup5,data=diamonds,range)
rank2group <- function (y,k=4){
    x=y[!is.na(y)]
    count = length(x)
    z = rank(x, ties.method = "min")
    y[!is.na(y)]=(floor((z - 1)/(count/k)) + 1)
    y
}
