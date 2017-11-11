#' Export to csv file for class "mytable" or "cbind.mytable"
#'
#' @param x An object of class "mytable" or "cbind.mytable"
#' @param row.names either a logical value indicating whether the row names of x
#'                  are to be written along with x, or a character vector of
#'                  row names to be written.
#' @param ... further arguments passed to or from other methods.
#' @export
#' @examples
#' require(moonBook)
#' res=mytable(sex~age+DM,data=acs)
#' mycsv(res,file="test.csv")
#' mycsv(summary(res),file="testsummary.csv")
mycsv=function(x,row.names=FALSE,...) UseMethod("mycsv")


#' Export to csv file for class "mytable"
#'
#' @param x An object of class "mytable" a result of a call to \code{\link{mytable}}
#' @param row.names either a logical value indicating whether the row names of x
#'                  are to be written along with x, or a character vector of
#'                  row names to be written.
#' @param ... further arguments passed to or from other methods.
#' @importFrom utils write.csv
#' @export
#' @examples
#' require(moonBook)
#' res=mytable(sex~age+DM,data=acs)
#' mycsv(res,file="test.csv")
#' mycsv(summary(res),file="testsummary.csv")
#' mycsv=function(x,row.names=FALSE) UseMethod("mycsv")
mycsv.mytable=function(x,row.names=FALSE,...) {
    out=mytable2df(x)
    write.csv(out,row.names=row.names,...)
}

#' Export to csv file for class "cbind.mytable"
#'
#' @param x An object of class "cbind.mytable" a result of a call to \code{\link{mytable}}
#' @param row.names either a logical value indicating whether the row names of x
#'                  are to be written along with x, or a character vector of
#'                  row names to be written.
#' @param ... further arguments passed to or from other methods.
#' @importFrom utils write.table
#' @export
#' @examples
#' require(moonBook)
#' res1=mytable(sex+Dx~age+DM,data=acs)
#' mycsv(res1,file="test1.csv")
#' mycsv(summary(res1),file="testsummary1.csv")
mycsv.cbind.mytable=function(x,row.names=FALSE,...) {

    myobj=x
    tcount=length(myobj) # number of tables
    tnames=unlist(attr(myobj,"caption"))
    group=attr(myobj,"group")

    out1=mytable2df(myobj[[1]])
    out2=mytable2df(myobj[[2]])
    result=cbind(out1,out2[,-1])

    if(tcount>2){
        for(i in 3:tcount){
            out=mytable2df(myobj[[i]])
            result=cbind(result,out[,-1])
        }
    }
    head=group[1]
    for(i in 1:tcount) {
        head=c(head,tnames[i],rep("",ncol(out1)-2))
    }
    result2=rbind(head,colnames(result),result)
    write.table(result2,row.names=row.names,col.names=FALSE,sep=",",...)
}


#' Convert mytable object to data.frame
#'
#'Add N number into data.frame
#' @param x An object of class "mytable" a result of a call to \code{\link{mytable}}
#'
#' @export
#' @return a data.frame with N number
mytable2df=function(x){
    if(x$show.all==TRUE) out=x$res
    else out=x$res[1:(length(x$res)-7)]
    ncount=c("",paste("(N=",x$count,")",sep=""),rep("",ncol(out)-3))
    out=rbind(ncount,out)
    out
}

