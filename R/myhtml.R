#' Export to html file for class "mytable" or "cbind.mytable" of "data.frame"
#'
#' @param x An object of class "mytable" or "cbind.mytable"
#' @param caption A character
#' @param rownames A logical value whether or not include rownames in table
#' @export
#' @examples
#' require(moonBook)
#' res=mytable(sex~age+Dx,data=acs)
#' myhtml(res)
#' res1=mytable(sex+Dx~.,data=acs)
#' myhtml(res1)
#' x=head(iris)
#' myhtml(x)
#' myhtml(x,caption="Table 1. myhtml Test")
#' myhtml(x,caption="Table 1. myhtml Test",rownames=FALSE)
myhtml=function(x,caption=NULL,rownames=TRUE) UseMethod("myhtml")


#' @describeIn myhtml
#'
#' @export
myhtml.default=function(x,caption=NULL,rownames=TRUE){
    cat("myhtml function only applicable to data.frame, mytable or cbind.mytable\n")
}


#' @describeIn myhtml
#'
#' @export
myhtml.mytable=function(x,caption=NULL,rownames=TRUE){
    out=mytable2html(x)
    if(is.null(caption))
        caption=paste("Descriptive Statistics by '",colnames(out)[1],"'",sep="")
    myhtmlHead()
    cat("<table cellpadding=10 cellspacing=5>")
    cat(paste("<caption>",caption,"</caption>",sep=""))
    cat("<tr>\n")
    #cat("<th></th>")
    for(i in 1:ncol(out)) {
        cat(paste("<th>",colnames(out)[i],"</th>",sep=""))
    }
    cat("</tr>\n")
    for(j in 1:nrow(out)){
        cat("<tr>")
        temp=gsub(" -","&nbsp;&nbsp;&nbsp;",out[j,1],fixed=TRUE)
        cat(paste("<td>",temp,"</td>",sep=""))
        for(i in 2:ncol(out)) {
            temp=gsub(" -","&nbsp;&nbsp;&nbsp;",out[j,i],fixed=TRUE)
            cat(paste("<td>",temp,"</td>",sep=""))
        }
        cat("</tr>\n")
    }
    cat("</table>\n")
}


#' @describeIn myhtml
#'
#' @export
myhtml.cbind.mytable=function(x,caption=NULL,rownames=TRUE){
    myobj=x
    tcount=length(myobj) # number of tables
    tnames=unlist(attr(myobj,"caption"))
    group=attr(myobj,"group")

    out1=mytable2html(myobj[[1]])
    out2=mytable2html(myobj[[2]])
    result=cbind(out1,out2[,-1])

    if(tcount>2){
        for(i in 3:tcount){
            out=mytable2html(myobj[[i]])
            result=cbind(result,out[,-1])
        }
    }

    if(is.null(caption)) {
        caption=paste("Descriptive Statistics stratified by ",group[1],sep="")
        for(i in 2:length(group)) caption=paste(caption," and ",group[i],sep="")
    }
    myhtmlHead()
    cat("<table cellpadding=5 cellspacing=5>\n")
    cat(paste("<caption>",caption,"</caption>\n",sep=""))
    cat("<tr>\n")
    cat(paste("<th>",group[1],"</th>",sep=""))
    for(i in 1:tcount) {
        cat(paste("<th colspan=\"",ncol(out1)-1,"\">",tnames[i],"</th>",sep=""))
    }
    cat("</tr>\n")
    cat("<tr>\n")
    for(i in 1:ncol(result)) {
        cat(paste("<th>",colnames(result)[i],"</th>",sep=""))
    }
    cat("</tr>\n")
    for(j in 1:nrow(result)){
        cat("<tr>")
        for(i in 1:ncol(result)) {
            temp=gsub(" -","&nbsp;&nbsp;&nbsp;",result[j,i],fixed=TRUE)
            cat(paste("<td>",temp,"</td>",sep=""))
        }
        cat("</tr>\n")
    }
    cat("</table>\n")
}


#' Prepare mytable object to data.frame ready to html
#'
#' Add N number into data.frame
#' @param x An object of class "mytable" a result of a call to \code{\link{mytable}}
#'
#' @export
#' @return a data.frame with N number
mytable2html=function(x){
    if(x$show.all==TRUE) out=x$res
    else out=x$res[1:(length(x$res)-7)]
    ncount=c("",paste("(N=",x$count,")",sep=""),rep("",ncol(out)-3))
    newcolnames=c("")
    ncount=length(x$count)
    for(i in 1:ncount){
        newcolnames[i+1]=paste(colnames(out)[i+1],paste("(N=",x$count[i],")",sep=""),
                               sep="<br/>")
    }
    for(i in 1:ncount+1) colnames(out)[i]=newcolnames[i]
    out
}



#' Print my html style
myhtmlHead=function(){
    cat("<head>")
    #cat("<style>
    #    table, th, td {
    #    border-top: 1px solid #bcbcbc;
    #    border-bottom: 1px solid #bcbcbc;
    #} </style>")
    cat("<style>
        table, th, td {
        border: 1px solid #bcbcbc;
    } </style>")
    cat("</head>\n")
}

