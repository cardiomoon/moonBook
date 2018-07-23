#'@describeIn mytable S3 method for data.frame
#'@export
#'@examples
#'mytable(acs)
mytable.data.frame=function(x,...){
    mytable_df(x,...)
}

#' make mytable from data.frame
#' @param x A data.frame
#' @param use.labels Logical. Whether or not use labels.
#' @param use.column.label Logical. Whether or not use column labels.
#' @param max.ylev An integer indicating the maximum number of levels of grouping
#'                 variable. If a colummn have unique values less than max.ylev
#'                 it is treated as a categorical variable. Default value is 5.
#' @param maxCatLevel An integer indicating the maximum number of unique levels of categorial variable.
#'                  If a colummn have unique values more than maxCatLevel, categorical summarization
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
#' @param show.all A logical value indicating whether or not all statistical
#'                 values have to be shown in table. Default value is FALSE.
#' @return An object of class "mytable.df".
#'      'print' returns a table for descriptive statistics.
#' @export
mytable_df=function(x,use.labels=TRUE,use.column.label=TRUE,max.ylev=5,maxCatLevel=20,digits=1,method=1,show.all=FALSE) {

     # x=acs[2];use.labels=TRUE;use.column.label=TRUE;max.ylev=5;maxCatLevel=20;
     # digits=1;method=3;show.all=TRUE

    name=c()
    no=c()
    out1=c()
    out2=c()
    out3=c()
    p=c()
    class=c()
    # str(x)

    plusminus="\u00b1"

    if(use.column.label){
        x<-changeColnameLabel(x)
    }
    if(use.labels){
        x<-addLabelDf(x)
    }
    for(i in 1:ncol(x)) {

        xname=colnames(x)[i]
        y=x[[xname]]

        if(is.numeric(y)){
            xlev=length(unique(y))
            kind=ifelse(xlev<=max.ylev,"categorical","numeric")
        } else{
            kind="categorical"
        }
        kind
        if(kind=="numeric") {

            temp=unlist(num_summary(y))

            if(method==3){
                if(nrow(x)<=5000) {
                    fit=shapiro.test(y)

                } else {
                    fit=nortest::ad.test(y)
                }
                statmethod=ifelse(fit$p.value<0.05,2,1)
                p=c(p,sprintf("%.3f",fit$p.value))
            } else{
                statmethod=method
                p=c(p,"")
            }
            form=paste0("%0.",digits,"f")
            name=c(name,xname)
            no=c(no,length(y)-sum(is.na(y)))
            if(statmethod==1) {

                out1=c(out1,sprintf(form,temp[1]))
                out2=c(out2,plusminus)
                out3=c(out3,sprintf(form,temp[2]))
            } else {
                out1=c(out1,sprintf(form,temp[3]))
                out2=c(out2,"")
                res=paste0("[",sprintf(form,temp[7]),";",sprintf(form,temp[9]),"]")
                out3=c(out3,res)
            }
            class=c(class,"continuous")
        } else {

            name=c(name,xname)
            no=c(no,length(y)-sum(is.na(y)))

            if(length(unique(y))>maxCatLevel){
                if("Date" %in% class(y)){
                    out1=c(out1,paste0("Date:",min(y)))
                    out2=c(out2,"-")
                    out3=c(out3,paste0(max(y)))
                } else{
                out1=c(out1,"unique values")
                out2=c(out2,"")
                out3=c(out3,length(unique(y)))
                }
                p=c(p,"")
                class=c(class,"categorical")
            }else{
                out1=c(out1,"")
                out2=c(out2,"")
                out3=c(out3,"")
                p=c(p,"")
                class=c(class,"categorical")
            res1=table(y)
            res1
            res2=prop.table(res1)*100
            res=rbind(res1,res2)
            res=as.data.frame(t(res))
            colnames(res)=c("Freq","Ratio")
            form=paste0("%0.",digits,"f")
            res$Ratio=paste0("(",sprintf(form,res$Ratio),"%)")

            for(j in 1:nrow(res)){
                name=c(name,paste0("  - ",rownames(res)[j]))
                no=c(no,"")
                out1=c(out1,res$Freq[j])
                out2=c(out2,"")
                out3=c(out3,res$Ratio[j])
                p=c(p,"")
                class=c(class,"")
            }
        }


        }

    }
    stats=paste(out1,out2,out3)

    result=data.frame(name=name,N=no,stats=stats,class=class,p=p,stringsAsFactors = FALSE)

    fmt=paste0("%-",max(nchar(result$name)),"s")
    result$name=sprintf(fmt,result$name)
    if(show.all==FALSE) {
        result=result[-ncol(result)]
    }
    class(result)=c("mytable.df","data.frame")
    result

}

#' Print an object of mytable.df
#' @param x An object of class mytable.df
#' @param ... Further arguments
#' @export
print.mytable.df=function(x,...){

    result<-x
    x1=x[-which(colnames(x)=="class")]
    length=apply(x1,2,function(y){max(nchar(as.character(y)),na.rm=TRUE)})
    fmt=paste0("%",length+1,"s")
    fmt
    string=paste(sprintf(fmt[1],result$name),sprintf(fmt[2],result$N),sprintf(fmt[3],result$stats))
    if(!is.null(result$p)) string=paste(string,result$p)
    string=paste(string,"\n")
    len=sum(length)+5
    cat("\n")
    cat(centerprint("Descriptive Statistics",width=len))
    cat("\n")
    cat(reprint("-",len),"\n")
    cat(paste(reprint(" ",length[1]+1),centerprint("N",width=length[2]+1),
              centerprint("Total",width=length[3]+1)))
    if(!is.null(result$p)) cat("  p ")
    cat("\n")
    cat(reprint("-",len),"\n")
    for(i in 1:length(string)) cat(string[i])
    cat(reprint("-",len),"\n")
}

