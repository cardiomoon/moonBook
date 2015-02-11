#'Demographic data of 857 patients with ACS
#'
#'A dataset containing demographic data and laboratory data of 857 pateints with
#'acute coronary syndrome(ACS).
#'
#'
#'@format A data frame with 857 rows and 17 variables:
#'\describe{
#'  \item{age}{patient age in years}
#'  \item{sex}{"Male" or "Female"}
#'  \item{cardiogenicShock}{"No" or "Yes"}
#'  \item{entry}{vascular access route, either "Femoral" or "Radial"}
#'  \item{Dx}{Final diagnosis, One of the followings : STEMI, NSTEMI or Unstable Angina}
#'  \item{EF}{ejection fraction, percentage by echocardiography}
#'  \item{height}{height in centimeter}
#'  \item{weight}{weight in kilogram}
#'  \item{BMI}{body mass index in kg/m2}
#'  \item{obesity}{obesity, "No" or "Yes"}
#'  \item{TC}{total cholesterol level in mg/dL}
#'  \item{LDLC}{low density lipoprotein cholesterol level in mg/dL}
#'  \item{HDLC}{high density lipoprotein cholesterol level in mg/dL}
#'  \item{TG}{triglyceride level in mg/dL}
#'  \item{DM}{history of diabetes mellitus,"No" or "Yes"}
#'  \item{HBP}{history of hypertension,"No" or "Yes"}
#'  \item{smoking}{history of smoking, One of the followings : "Never","Ex-smoker","Smoker"}
#'}
#'@name acs
NULL

#'Demographic data of 115 patients performing IVUS(intravascular ultrasound)
#'examination of a radial artery.
#'
#'A dataset containing demographic data and laboratory data of 115 pateints performing IVUS(intravascular ultrasound)
#'examination of a radial artery after tansradial coronary angiography.
#'
#'
#'@format A data frame with 115 rows and 15 variables:
#'\describe{
#'  \item{male}{if Male, 1; if Female 0}
#'  \item{age}{patient age in years}
#'  \item{height}{height in centimeter}
#'  \item{weight}{weight in kilogram}
#'  \item{HBP}{history of hypertension, 1 for yes or 0 for no}
#'  \item{DM}{history of diabetes mellitus, 1 for yes or 0 for no}
#'  \item{smoking}{history of smoking, One of the followings : "non-smoker","ex-smoker","smoker"}
#'  \item{TC}{total cholesterol level in mg/dL}
#'  \item{TG}{triglyceride level in mg/dL}
#'  \item{HDL}{high density lipoprotein cholesterol level in mg/dL}
#'  \item{LDL}{low density lipoprotein cholesterol level in mg/dL}
#'  \item{hsCRP}{high-sensitive C reactive protein}
#'  \item{NTAV}{normalized total atheroma volume measured by IVUS in cubic mm}
#'  \item{PAV}{percent atheroma volume in percentage}
#'  \item{sex}{Factor with two levels; "Male" or "Female"}
#'}
#'@name radial
NULL




#' Exporting "cbind.mytable","mytable" to LaTeX format
#'
#' Exporting "cbind.mytable","mytable" to LaTeX format
#'@param myobj An object of class 'mytable'
#'@param size An integer indicating font size, defaulting is 5.
#'@param caption A character
#'@param caption.placement The caption will be have placed at the top of the table
#'        if caption.placement is "top" and at the bottom of the table
#'        if it equals "bottom". Default value is "top".
#'@param caption.position The caption will be have placed at the center of the table
#'        if caption.position is "center" or "c", and at the left side of the table
#'        if it equals "left" or "l", and at the right side of the table
#'        if it equals "right" or "r". Default value is "center".
#'@examples
#' require(moonBook)
#' out=mytable(sex~.,data=acs)
#' mylatex(out)
#' out1=mytable(sex+Dx~.,data=acs)
#' mylatex(out1,size=6)
mylatex=function(myobj,size=5,caption=NULL,caption.placement="top",
                 caption.position="c") UseMethod("mylatex")


#'@describeIn mylatex
mylatex.default=function(myobj,size=5,caption=NULL,caption.placement="top",
                         caption.position="c") {

    cat("mylatex function only applicable to data.frame, mytable or cbind.mytable\n")
}


#'@describeIn mylatex
mylatex.mytable=function(myobj,size=5,caption=NULL,caption.placement="top",
                         caption.position="c") {

    ## Generate latex table for class mytable
    result=obj2linecount(myobj)
    y=result$y
    out1=result$out1
    cn=result$cn
    ncount=result$ncount
    col.length=result$col.length
    linelength=result$linelength

    if(identical(caption.placement,"bottom") | identical(caption.placement,"b"))
            caption.placement="bottom"
    else caption.placement="top"
    if(identical(caption.position,"left")|identical(caption.position,"l"))
            caption.position="l"
    else if(identical(caption.position,"right")|identical(caption.position,"r"))
            caption.position="r"
    else caption.position="c"

    if(!is.numeric(size)) size=5
    else if(size<0 | size>10) size=5
    Fontsize=c("tiny","scriptsize","footnotesize","small","normalsize",
               "large","Large","LARGE","huge","Huge")
    cat("\\begin{table}[!hbp]\n")

    cat(paste("\\begin{",Fontsize[size],"}\n",sep=""))
    head=c("\\begin{tabular}{l")
    for(i in 2 : length(cn)) { head=paste(head,"c",sep="")}
    head=paste(head,"}\n",sep="")
    cat(head)
    if(is.null(caption)) caption= paste("Descriptive Statistics by ",y,sep="")
    if(caption.placement=="top")
        cat(paste("\\multicolumn{",length(cn),"}{",
                  caption.position,"}{",caption,"}\\\\ \n",sep=""))
    cat("\\hline\n")
    firstrow=cn[1]
    for(i in 2:length(ncount)) { firstrow=paste(firstrow,cn[i],sep=" & ")}
    for(i in 1:(length(cn)-length(ncount))){
        firstrow=paste(firstrow," & \\multirow{2}{*}{",
                       cn[length(ncount)+i],"}",sep="")
    }
    cat(paste(firstrow,"\\\\ \n",sep=""))

    secondrow=ncount[1]
    for(i in 2:length(ncount)) {
        secondrow=paste(secondrow,ncount[i],sep=" & ")
    }
    for(i in 1:(length(cn)-length(ncount)))
        secondrow=paste(secondrow," & ",sep="")
    cat(paste(secondrow," \\\\ \n",sep=""))
    cat("\\hline\n")

    for(i in 1:dim(out1)[1]){
        temp=r(out1[i,1])
        for(j in 2:(length(cn))) {
            temp=paste(temp,r(out1[i,j]),sep=" & ")
        }
        cat(paste(temp,"\\\\ \n",sep=""))
    }
    cat("\\hline\n")
    if(caption.placement=="bottom")
        cat(paste("\\multicolumn{",length(cn),"}{",
                  caption.position,"}{",caption,"}\\\\ \n",sep=""))
    cat("\\end{tabular}\n")
    cat(paste("\\end{",Fontsize[size],"}\n",sep=""))
    cat("\\end{table}\n")
}

#'Subfunction used in mylatex
#'
#' @param string a character vector
r=function(string) {
    string=gsub("%","\\%",string,fixed=TRUE)
    string
}

#'@describeIn mylatex
mylatex.cbind.mytable=function(myobj,size=5,caption=NULL,
                               caption.placement="top",caption.position="c"){
    ## Generate latex table for cbind.mytable

    tcount=length(myobj) # number of tables
    tnames=unlist(attr(myobj,"caption"))
    group=attr(myobj,"group")
    result=list()

    for(i in 1:tcount) result[[i]]=obj2linecount(myobj[[i]])
    if(!is.numeric(size)) size=5
    else if(size<0 | size>10) size=5

    Fontsize=c("tiny","scriptsize","footnotesize","small","normalsize",
               "large","Large","LARGE","huge","Huge")
    cat("\\begin{table}[!hbp]\n")
    cat(paste("\\begin{",Fontsize[size],"}\n",sep=""))
    cat("\\centering\n")
    if(is.null(caption)) {
        caption=paste("Descriptive Statistics stratified by ",group[1],sep="")
        for(i in 2:tcount) caption=paste(caption," and ",group[i],sep="")
    }

    if(identical(caption.placement,"bottom") | identical(caption.placement,"b"))
        caption.placement="bottom"
    else caption.placement="top"
    if(identical(caption.position,"left")|identical(caption.position,"l"))
        caption.position="l"
    else if(identical(caption.position,"right")|identical(caption.position,"r"))
        caption.position="r"
    else caption.position="c"

    colno=length(result[[1]]$cn)
    # number of total column
    tcn=colno*tcount

    head=c("\\begin{tabular}{l")
    for(i in 2 : tcn) { head=paste(head,"c",sep="")}
    head=paste(head,"}\n",sep="")
    cat(head)
    if(caption.placement=="top")
        cat(paste("\\multicolumn{",tcn,"}{",
                  caption.position,"}{",caption,"}\\\\ \n",sep=""))
    cat("\\hline\n")

    temp=paste(" & \\multicolumn{",colno-1,"}{c}{",
               tnames[1],"} ",sep="")
    for(i in 2:tcount){
        #if(class(tnames[i])=="factor") temp=levels(tnames)[i]
        #else temp=tnames[i]
        temp=paste(temp," &  &\\multicolumn{",colno-1,"}{c}{",
                   tnames[i],"}",sep="")
    }
    temp=paste(temp," \\\\ \n",sep="")
    cat(temp)

    temp=paste("\\cline{2-",colno,"}",sep="")
    for(i in 2:tcount) {
        start=colno+2+(i-2)*(colno)
        temp=paste(temp,"\\cline{",start,"-",start+colno-2,"}",sep="")
    }
    cat(temp,"\n")
    temp=" "
    for(i in 1:tcount) {
        for(j in 1:(length(result[[i]]$cn))) {
            if(j==1) {
                if(i>1) temp=paste(temp," & ",sep="")
            }
            else temp=paste(temp," & ",result[[i]]$cn[j],sep="")
        }
    }
    cat(temp,"\\\\ \n")
    temp=" "
    for(i in 1:tcount) {
        for(j in 1:(length(result[[i]]$ncount))) {
            if(j==1) {
                if(i>1) temp=paste(temp," & ",sep="")
            }
            else temp=paste(temp,result[[i]]$ncount[j],sep=" & ")
        }
        for(k in 1:(colno-length(result[[1]]$ncount)))
            temp=paste(temp," & ",sep="")
    }
    cat(temp,"\\\\ \n")
    cat("\\hline\n")
    for(i in 1:dim(result[[1]]$out1)[1]){
        temp=""
        for(k in 1:tcount){
            if(k==1) {
                for(j in 1:colno) {
                    if(j==1) temp=r(result[[k]]$out1[i,j])
                    else temp=paste(temp,r(result[[k]]$out1[i,j]),sep=" & ")
                }
            }
            else {
                for(j in 1:colno){
                    if(j==1) temp=paste(temp," & ",sep="")
                    else temp=paste(temp,r(result[[k]]$out1[i,j]),sep=" & ")
                }
            }
        }
        cat(temp,"\\\\ \n")
    }
    cat("\\hline\n")
    if(caption.placement=="bottom")
        cat(paste("\\multicolumn{",tcn,"}{",
                  caption.position,"}{",caption,"}\\\\ \n",sep=""))
    cat("\\end{tabular}\n")
    cat(paste("\\end{",Fontsize[size],"}\n",sep=""))
    cat("\\end{table}\n")
}
