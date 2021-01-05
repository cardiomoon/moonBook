## ----comment=NA----------------------------------------------------------
require(moonBook)
data(acs)
mytable(Dx~.,data=acs)

## ----comment=NA----------------------------------------------------------
str(acs)

## ----comment=NA----------------------------------------------------------
mytable(acs)

## ----comment=NA----------------------------------------------------------
mytable(~age+sex,data=acs)

## ----comment=NA----------------------------------------------------------
if(!require(sjlabelled)) {
    install.packages("sjlabelled")
    require(sjlabelled)
}
df=mtcars
df$am<-set_label(df$am,label="Transmission")
df$am<-set_labels(df$am,labels=c("automatic","manual"))
df$vs<-set_label(df$vs,label="Engine")
df$vs<-set_labels(df$vs,labels=c("V-shaped","straight"))

## ----comment=NA----------------------------------------------------------
str(df)

## ----comment=NA----------------------------------------------------------
mytable(df)
mytable(df,use.labels=FALSE, use.column.label=FALSE)
mytable(am~.,data=df)
# mytable(vs+am~.,data=df)

## ----comment=NA----------------------------------------------------------
mytable(sex~age+Dx,data=acs)

## ---- comment=NA---------------------------------------------------------
mytable(am~.-hp-disp-cyl-carb-gear,data=mtcars)

## ----comment=NA----------------------------------------------------------
mytable(sex~height+weight+BMI,data=acs,method=3)

## ---- comment=NA,warning=FALSE-------------------------------------------
mytable(am~.,data=mtcars)

## ----comment=NA,warning=FALSE--------------------------------------------
mytable(am~carb,data=mtcars,max.ylev=6)

## ----comment=NA,warning=FALSE--------------------------------------------
mytable(sex+DM~.,data=acs)


## ----results='asis'------------------------------------------------------
out=mytable(Dx~.,data=acs)
myhtml(out)
out1=mytable(sex+DM~.,data=acs)
myhtml(out1)

## ----eval=FALSE----------------------------------------------------------
#  mylatex(mytable(sex+DM~age+Dx,data=acs))

## ----eval=FALSE----------------------------------------------------------
#  out=mytable(sex~age+Dx,data=acs)
#  for(i in c(3,5))
#      mylatex(out,size=i,caption=paste("Table ",i,". Fontsize=",i,sep=""))

## ----eval=FALSE----------------------------------------------------------
#  mycsv(out,file="test.csv")
#  mycsv(out1,fil="test1.csv")

## ----messgae=FALSE,results='asis'----------------------------------------
require(ztable)
require(magrittr)
mytable(sex+DM~.,data=acs) %>%
    ztable %>%
    addSigColor %>%
    print(type="html")

## ----fig.height=5,fig.width=6--------------------------------------------
library(moonBook)
densityplot(age~sex,data=acs)
densityplot(age~Dx,data=acs)

## ----fig.width=6,fig.height=6,comment=NA---------------------------------
require(survival)
data(colon)
out1=glm(status~sex+age+rx+obstruct+node4,data=colon)
out2=glm(status~rx+node4,data=colon)
ORplot(out1,type=2,show.CI=TRUE,xlab="This is xlab",main="Odds Ratio")
ORplot(out2,type=1)
ORplot(out1,type=1,show.CI=TRUE,col=c("blue","red"))
ORplot(out1,type=4,show.CI=TRUE,sig.level=0.05)
ORplot(out1,type=1,show.CI=TRUE,main="Odds Ratio",sig.level=0.05,
        pch=1,cex=2,lwd=4,col=c("red","blue"))

## ----fig.width=6,fig.height=6,comment=NA---------------------------------
attach(colon)
colon$TS=Surv(time,status==1)
out=mycph(TS~.,data=colon)
out
HRplot(out,type=2,show.CI=TRUE,cex=2,sig=0.05,
       main="Hazard ratios of all individual variables")

