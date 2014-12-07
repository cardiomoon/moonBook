#' @describeIn mylatex
#'
mylatex.data.frame=function(myobj,size=5,caption=NULL,rownames=TRUE,
    caption.placement="top",caption.position="c",
    align=NULL,digits=NULL){

    ncount=ncol(x)
    nrow=nrow(x)
    cn=colnames(x)
    if(!is.null(caption)) {
        if(identical(caption.placement,"bottom") | identical(caption.placement,"b"))
            caption.placement="bottom"
        else caption.placement="top"
        if(identical(caption.position,"left")|identical(caption.position,"l"))
            caption.position="l"
        else if(identical(caption.position,"right")|identical(caption.position,"r"))
                caption.position="r"
        else caption.position="c"
    }
    logicals <- unlist(lapply(x, is.logical))
    x[, logicals] <- lapply(x[, logicals, drop = FALSE], as.character)
    characters <- unlist(lapply(x, is.character))
    factors <- unlist(lapply(x, is.factor))
    ints <- sapply(x, is.integer)
    if(is.null(align)){
        y <- c("r", c("r","l")[(characters | factors) + 1])
        for(i in (2-rownames):length(y)) align=paste(align,y[i],sep="")
    }
    if(length(digits)==1) digits=rep(digits,ncount+1)
    digits <- switch(1 + is.null(digits), digits, c(0, rep(2,ncol(x))))
    display=NULL
    if (is.null(display)) {
        display <- rep("f", ncol(x))
        display[ints] <- "d"
        display[characters | factors] <- "s"
        display <- c("s", display)
    }
    if(!is.numeric(size)) size=5
    else if(size<0 | size>10) size=5
    Fontsize=c("tiny","scriptsize","footnotesize","small","normalsize",
           "large","Large","LARGE","huge","Huge")
    cat("\\begin{table}[!hbp]\n")
    cat(paste("\\begin{",Fontsize[size],"}\n",sep=""))
    cat(paste("\\begin{tabular}{",align,"}\n",sep=""))
    if(!is.null(caption) & caption.placement=="top")
        cat(paste("\\multicolumn{",ncount+rownames,"}{",
                  caption.position,"}{",caption,"}\\\\ \n",sep=""))
    cat("\\hline\n")
    if(rownames) firstrow=paste("&",cn[1])
    else firstrow=cn[1]
    for(i in 2:ncount) { firstrow=paste(firstrow,cn[i],sep=" & ")}
    cat(paste(firstrow,"\\\\ \n",sep=""))
    cat("\\hline\n")
    for(i in 1:nrow){
        temp=c()
        if(rownames) temp=rownames(x)[i]
        for(j in 1:ncount) {
            if(display[j+1]=="f") fmt=paste("%.",digits[j+1],display[j+1],sep="")
            else fmt=paste("%",display[j+1],sep="")

            if(is.null(temp)) temp=sprintf(fmt,x[i,j])
            else temp=paste(temp,sprintf(fmt,x[i,j]),sep=" & ")
        }
        cat(paste(temp,"\\\\ \n",sep=""))
    }
    cat("\\hline\n")
    if(!is.null(caption) & caption.placement=="bottom")
        cat(paste("\\multicolumn{",ncount+rownames,"}{",
                  caption.position,"}{",caption,"}\\\\ \n",sep=""))
    cat("\\end{tabular}\n")
    cat(paste("\\end{",Fontsize[size],"}\n",sep=""))
    cat("\\end{table}\n")
}


#' @describeIn myhtml
#'
myhtml.data.frame=function(x,caption=NULL,rownames=TRUE){
    myhtmlHead()
    cat("<table cellpadding=10 cellspacing=5>")
    cat(paste("<caption>",caption,"</caption>",sep=""))
    cat("<tr>\n")
    if(rownames) cat("<th>","","</th>")
    for(i in 1:ncol(x)) {
        cat(paste("<th>",colnames(x)[i],"</th>",sep=""))
    }
    cat("</tr>\n")
    for(j in 1:nrow(x)){
        cat("<tr>")
        if(rownames) cat(paste("<td>",rownames(x)[j],"</td>",sep=""))
        for(i in 1:ncol(x)) cat(paste("<td>",x[j,i],"</td>",sep=""))
        cat("</tr>\n")
    }
    cat("</table>\n")
}


