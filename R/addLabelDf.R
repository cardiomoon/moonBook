#'Add labels to the data.frame
#'@param data A data.frame
#'@importFrom purrr map_df
#'@importFrom sjmisc to_label
#'@return A data.frame
#'@export
addLabelDf=function(data){
        data<-purrr::map_df(data,function(x) to_label(x,add.non.labelled=TRUE))
}


#'Add column labels to the data.frame
#'@param data A data.frame
#'@param formula A formula
#'@return A data.frame
#'@export
changeColnameLabel=function(data,formula=NULL){
    pos=c()

    if(!is.null(formula)){
        left=extractVars(formula,data)$left
        right=setdiff(colnames(data),left)
        for(i in 1:length(right)){
            pos=c(pos,grep(right[i],colnames(data)))
        }
    } else{
        pos=1:ncol(data)
    }
    for(i in 1:ncol(data)){
        if(i %in% pos) {
            label=attr(data[[i]],"label")
            if(!is.null(label)) colnames(data)[i]=label
        }

    }

    data
}

#'Extract varaiable names from formula
#'@param formula a formula
#'@param data a data.frame
#'@export
extractVars=function(formula,data){

    call = paste(deparse(formula), ", ", "data= ", substitute(data), sep = "")
    cat("call=",call,"\n")
    f = formula
    myt = terms(f, data = data)
    left = as.character(f[[2]])
    if(length(left)>1) left=left[-1]
    left
    right = as.character(f[[3]])
    list(left=left,right=right)
}

# kpci2016=readRDS("~/ownCloud/Documents/KPCI/kpci2016.RDS")
#
# data=readRDS("~/ownCloud/Documents/betam4/data/JCI인증.RDS")
#
# str(data)
# mytable(data)
# data1=data.frame(addLabelDf(data))
# mytable(data1)
# data2=changeColnameLabel(data1)
# colnames(data2)
# df=mtcars
# require(sjlabelled)
# df$am<-set_label(df$am,label="Transmission")
# df$am<-set_labels(df$am,labels=c("automatic","manual"))
# df$vs<-set_label(df$vs,label="Engine")
# df$vs<-set_labels(df$vs,labels=c("V-shaped","straight"))
#
# mytable(df)
# str(df)
# mytable(vs~.,data=df)
