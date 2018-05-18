#'Add labels to the data.frame
#'@param data A data.frame
#'@importFrom sjmisc to_label
#'@return A data.frame
#'@export
addLabelDf=function(data){
        data.frame(lapply(data,function(x) to_label(x,add.non.labelled=TRUE)))
}


#'Add column labels to the data.frame
#'@param data A data.frame
#'@param colname character. column name
#'@param use.column.label Logical. Whether or not use column labels.
#'@export
getLabel=function(data,colname,use.column.label=TRUE){
    temp=c()
    if(use.column.label){
        for(i in 1:length(colname)){
        label=attr(data[[colname[i]]],"label",exact=TRUE)
        if(!is.null(label)) {
             temp=c(temp,label)
        }else {
             temp=c(temp,colname[i])
        }
        }
    } else{
        temp=colname
    }
    temp

}

#'Change column names with labels
#'@param data A data.frame
#'@export
changeColnameLabel=function(data){
    for(i in 1:ncol(data)){
        label=attr(data[[i]],"label",exact=TRUE)
        if(!is.null(label)) colnames(data)[i]=label
    }
    data
}




