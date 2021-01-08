#' Convert number to formatted number
#' @param x A numeric vector
#' @param ... Further arguments to be passed to function format
#' @export
comma <- function(x,...) format(x,  big.mark = ",",...)

#' Change numbers into formatted numbers
#' @param x An object
#' @export
#' @examples
#' \dontrun{
#' require(stringr)
#' require(magrittr)
#' require(ggplot2)
#' mytable(cut~.,data=diamonds) %>% addComma
#' x=mytable(Dx~sex,data=acs)
#' addComma(x)
#' }
addComma=function(x) UseMethod("addComma")


#' @describeIn addComma S3 method for class mytable
#' @export
addComma.mytable=function(x){
    x$res<-addComma(x$res)
    x$count=comma(x$count)
    x
}

#' @describeIn addComma S3 method for class mytable.df
#' @export
#' @importFrom stringr str_trim
addComma.mytable.df=function(x){

    x$N=comma(as.numeric(x$N))
    x$N[str_trim(x$N,side="both")=="NA"]=""
    x[[3]]<-addComma(x[[3]])
    x
}


#' @describeIn addComma S3 method for class cbind.mytable
#' @export
addComma.cbind.mytable=function(x){
    for(i in 1:length(x)){
        x[[i]]$res<-addComma(x[[i]]$res)
        x[[i]]$count=comma(x[[i]]$count)
    }

    x
}

#' @describeIn addComma S3 method for class data.frame
#' @export
addComma.data.frame=function(x){

    df<-x
    if(ncol(df)>8){
        select=2:(ncol(df)-8)
    } else{
        select=1:ncol(df)
    }

    select
    for(i in 1:length(select)){
        df[[select[i]]]=addComma(df[[select[i]]])
    }
    df
}

#' @describeIn addComma S3 method for class character
#' @importFrom stringr fixed str_detect str_flatten str_split str_replace
#' @importFrom magrittr "%>%"
#' @export
addComma.character=function(x){

    string<-x

    plusminus="\u00b1"

    select=str_detect(string,plusminus )
    string[select]<-string[select] %>%
        str_replace_all(" ","") %>%
        str_split(plusminus) %>%
        lapply(function(x) comma(as.numeric(x))) %>%
        lapply(function(x) str_replace_all(x," ","")) %>%
        lapply(function(x) str_flatten(x,collapse=paste0(" ",plusminus," "))) %>%
        unlist

    select=str_detect(string,fixed("("))
    string[select]<-string[select] %>%
        str_replace_all(fixed("%)"),"") %>%
        str_split(fixed("(")) %>%
        lapply(function(x) comma(as.numeric(x),digits=3)) %>%
        lapply(function(x) str_replace_all(x," ","")) %>%
        lapply(function(x) str_flatten(x,collapse=paste0(" ("))) %>%
        lapply(function(x) paste0(x,"%)")) %>%
        lapply(function(x) str_replace(x,fixed(".0 ("),fixed(" ("))) %>%
        unlist

    select=str_detect(string,":")
    string[select] <- string[select] %>%
        str_split(":") %>%
        lapply(function(x) {x[2]=comma(as.numeric(x[2]));x}) %>%
        lapply(function(x) str_flatten(x,collapse=":")) %>%
        unlist


    select=str_detect(string,fixed("["))
    string[select] <- string[select] %>%
        str_replace_all(fixed("["),",") %>%
        str_replace_all(fixed(";"),",") %>%
        str_replace_all(fixed("]"),"") %>%
        str_split(",") %>%
        lapply(function(x) comma(as.numeric(x))) %>%
        lapply(function(x) str_replace_all(x," ","")) %>%
        lapply(function(x) paste0(x[1]," [",x[2],";",x[3],"]")) %>%
        unlist

    string
}
