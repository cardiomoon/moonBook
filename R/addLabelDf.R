#' Add value labels to the data.frame
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@importFrom sjmisc to_label
#'@export
addLabelDf=function(data,mapping=NULL){

    if(!is.null(mapping)) {
        (mapnames=names(mapping))
        cols=c()
        for(i in 1:length(mapnames)) {
            temp=getMapping(mapping,mapnames[i])
            # if(length(temp)>1) temp=temp[-1]
            cols=c(cols,temp)
        }
        cols=unique(cols)
        data[cols]=lapply(data[cols],function(x) to_label(x,add.non.labelled=TRUE))
        # for(i in 1:length(cols)){
        #
        #         data[[cols[[i]]]]=to_label(data[[cols[i]]],add.non.labelled=TRUE)
        # }
    } else{
        # cols=colnames(data)
        # for(i in 1:length(cols)){
        #         data[[cols[[i]]]]=to_label(data[[cols[i]]],add.non.labelled=TRUE)
        # }
        data[]=lapply(data,function(x) to_label(x,add.non.labelled=TRUE))
    }
    data
}

#' extract variable name from mapping, aes
#' @param mapping aesthetic mapping
#' @param varname variable name to extract
#' @return variable name in character
#' @importFrom stringr str_replace_all str_detect str_split fixed
#' @importFrom utils packageVersion
#' @export
#' @examples
#' require(ggplot2)
#' mapping=aes(colour=sex)
#' getMapping(mapping,"colour")
#' getMapping(mapping,"x")
getMapping=function(mapping,varname) {

    # mapping=aes(colour=sex)
    # varname="x"
    if(is.null(mapping)) return(NULL)
    result=paste(mapping[varname])
    if(result=="NULL") result<-NULL
    if(!is.null(result)){
        if(packageVersion("ggplot2") > "2.2.1") {
            result=stringr::str_replace_all(result,"~","")
        }
        result=stringr::str_replace_all(result,stringr::fixed("c("),"")
        result=stringr::str_replace_all(result,stringr::fixed(")"),"")
        result=stringr::str_replace_all(result," ","")
        if(stringr::str_detect(result,",")) {
            result=unlist(stringr::str_split(result,","))
        }

    }
    result
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




