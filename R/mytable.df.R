#' make mytable from data.frame
#' @param data A data.frame
#' @param max.ylev An integer indicating the maximum number of levels of grouping
#'                 variable ('y'). If a colummn have unique values less than max.ylev
#'                 it is treated as a categorical variable. Default value is 5.
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
mytable.df=function(data,max.ylev=5,digits=1,method=1,show.all=FALSE) {

    name=c()
    out1=c()
    out2=c()
    out3=c()
    p=c()

    plusminus="\u00b1"
    for(i in 1:ncol(data)) {

        xname=colnames(data)[i]
        x=data[[xname]]
        if(is.numeric(x)){
            xlev=length(unique(x))
            kind=ifelse(xlev<=max.ylev,"categorical","numeric")
        } else{
            kind="categorical"
        }
        if(kind=="numeric") {
            temp=unlist(num_summary(x))

            if(method==3){
                if(length(x)<=5000) {
                    fit=shapiro.test(x)

                } else {
                    fit=nortest::ad.test(x)
                }
                statmethod=ifelse(fit$p.value<0.05,2,1)
                p=c(p,round(fit$p.value,3))
            } else{
                statmethod=method
                p=c(p,"")
            }
            form=paste0("%0.",digits,"f")
            name=c(name,xname)
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



        }
        else {
            res1=table(x)
            res2=prop.table(res1)*100
            res=rbind(res1,res2)
            res=as.data.frame(t(res))

            colnames(res)=c("Freq","Ratio")
            res$Ratio=paste0("(",sprintf(form,res$Ratio),"%)")
            name=c(name,xname)
            out1=c(out1,"")
            out2=c(out2,"")
            out3=c(out3,"")
            p=c(p,"")
            for(j in 1:nrow(res)){
                name=c(name,paste0("  - ",rownames(res)[j]))
                out1=c(out1,res$Freq[j])
                out2=c(out2,"")
                out3=c(out3,res$Ratio[j])
                p=c(p,"")
            }


        }

    }

    result=data.frame(name=name,out1=out1,out2=out2,out3=out3,p=p)
    fmt=paste0("%-",max(nchar(result$name)),"s")
    result$name=sprintf(fmt,result$name)
    if(show.all==FALSE) result=result[-ncol(result)]
    class(result)=c("mytable.df","data.frame")
    attr(result,"name")=substitute(data)
    result

}

#' Print an object of mytable.df
#' @param x An onject of class mytable.df
#' @param ... Further arguments
#' @importFrom stringr str_dup str_pad
#' @export
print.mytable.df=function(x,...){

    result<-x
    string=list()
    len1=0
    if(length(result$out1[result$out2!=""])>0) {
        len1=max(nchar(result$out1[result$out2!=""]),na.rm=TRUE)
    }

    if(len1>0) result$out1[result$out2!=""]=str_pad(result$out1[result$out2!=""],
                                         len1,side="left")
    result$out1[result$out2==""]=str_pad(result$out1[result$out2==""],
                                         len1,side="left")

    result$out3[result$out2==""]=str_pad(result$out3[result$out2==""],
                                         max(nchar(result$out3[result$out2==""])),side="left")
    if(len1>0) result$out3[result$out2!=""]=str_pad(result$out3[result$out2!=""],
                                         max(nchar(result$out3[result$out2!=""])),side="left")
    temp=paste(result$out1,result$out2,result$out3)
    temp
    #result[]=lapply(result,function(x) {str_pad(x,width=max(nchar(x)),side="left")})
    for(i in 1:nrow(result)){
        string[[i]]=paste(result$name[i],temp[i],"\n")
    }

    len=max(nchar(string))
    cat(paste0("Descriptive Statistics of data '",attr(result,"name"),"'\n"))
    cat(str_dup("-",len),"\n")
    lapply(string,cat)
    cat(str_dup("-",len),"\n")
}
