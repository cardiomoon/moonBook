#'Compress an object of class mytable or cbind.mytable
#'@param x An object of class mytable or cbind.mytable
#'@param no Representative group of two groups
#'@param add.label Logical. Whether or not add representative group name
#'@export
#'@examples
#'require(stringr)
#'require(ztable)
#'require(magrittr)
#'mytable(acs) %>% compress
#'mytable(Dx~.,data=acs) %>% compress
#'mytable(Dx~.,data=acs) %>% compress %>% ztable
#'mytable(Dx+sex~.,data=acs) %>% compress
compress=function(x,no=2,add.label=TRUE)  UseMethod("compress")


#' @describeIn compress S3 method for class mytable
#' @export
compress.mytable=function(x,no=2,add.label=TRUE){
     df<-x$res
     x$res<-compress(df,no=no,add.label=add.label)
     x
}

#' @describeIn compress S3 method for class cbind.mytable
#' @export
compress.cbind.mytable=function(x,no=2,add.label=TRUE){

    for(i in 1:length(x)){
        df<-x[[i]]$res
        x[[i]]$res<-compress(df,no=no,add.label=add.label)
    }
    x
}

#' @describeIn compress S3 method for class data.frame
#' @importFrom stringr str_replace_all str_pad
#' @export
compress.data.frame=function(x,no=2,add.label=TRUE){
    # x=mytable(acs);add.label=TRUE;no=2
    df<-x
    kind=extractKind(df)
    res<-df[0,]
    selected=c()
    for(i in 1:nrow(df)){
        # cat("i=",i,"\n")
        if(i %in% selected) {
            next
        } else if(kind[i]==1) {
            res=rbind(res,df[i,])
        } else if(kind[i]==2){
            temp=df[i,]
            if(add.label){
            temp[1,1]=paste0(df[[1]][i],df[[1]][i+no])
            temp[1,1]=str_replace_all(temp[1,1],"-",":")
            }
            temp[1,1]=str_replace_all(temp[1,1]," ","")
            if(ncol(df)>9){
                for(j in 1:(ncol(df)-9)){
                # cat("i=",i,",j=",j,"\n")
                    temp[1,j+1]=df[[j+1]][i+no]
                }
            } else{
                temp[1,3]=df[[3]][i+no]
            }
            res=rbind(res,temp)
            selected=c(selected,i,i+1,i+2)
        } else{
            res=rbind(res,df[i,])
        }

    }
    res[[1]]=str_pad(res[[1]],width=max(nchar(res[[1]])),side="right")
    res
}

#' Extract kind of an object of class mytable
#' @param df An object of class mytable or cbind.mytable
#' @export
extractKind=function(df){
    kind=c()
    for(i in 1:nrow(df)){

        if(df$class[i]=="") {
            kind=c(kind,0)
        } else if(df$class[i]=="continuous") {
            kind=c(kind,1)
        } else {
            temp=0
            for(j in (i+1):nrow(df)){

                if(df$class[j]=="") {
                    temp=temp+1
                } else break
            }
            kind=c(kind,temp)
        }
    }
    kind
}


#'Delete rows of class mytable object
#'@param x An object of class mytable or cbind.mytable
#'@param rows rows to delete
#'@export
deleteRows=function(x,rows){
    if("cbind.mytable" %in% class(x)){
        for(i in 1:length(x)){
            x[[i]]$res<-x[[i]]$res[-rows,]
        }
    } else if("mytable" %in% class(x)){
        x$res<-x$res[-rows,]
    }
    x
}

