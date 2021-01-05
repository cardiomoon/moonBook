pkgname <- "predict3d"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('predict3d')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_lines")
### * add_lines

flush(stderr()); flush(stdout())

### Name: add_lines
### Title: Add lines with labels to pre-existing ggplot
### Aliases: add_lines

### ** Examples

require(ggplot2)
fit=lm(mpg~wt*hp,data=mtcars)
df=calEquation(fit)
p=ggplot(data=mtcars,aes(x=wt,y=mpg))
add_lines(p,df)
add_lines(p,df,lty=1:3,color=1:3,size=1)
fit=lm(mpg~wt*vs,data=mtcars)
df=calEquation(fit)
p=ggplot(data=mtcars)+geom_point(aes(x=wt,y=mpg))
add_lines(p,df)
add_lines(p,df,lty=1:2,color=1:2,size=1)+theme_bw()



cleanEx()
nameEx("beNumeric")
### * beNumeric

flush(stderr()); flush(stdout())

### Name: beNumeric
### Title: Whether a string vector can be converted to numeric
### Aliases: beNumeric

### ** Examples

x=c("age","22.5","11/2")
beNumeric(x)



cleanEx()
nameEx("calEquation")
### * calEquation

flush(stderr()); flush(stdout())

### Name: calEquation
### Title: calculated slope and intercept from object of class lm
### Aliases: calEquation

### ** Examples

fit=lm(mpg~wt*hp+carb,data=mtcars)
calEquation(fit)
calEquation(fit,pred="hp")



cleanEx()
nameEx("fit2newdata")
### * fit2newdata

flush(stderr()); flush(stdout())

### Name: fit2newdata
### Title: Make a new data set for prediction
### Aliases: fit2newdata

### ** Examples

fit=lm(mpg~hp*wt*cyl+carb+am,data=mtcars)
fit2newdata(fit,predictors=c("hp","wt","am"))
fit2newdata(fit,predictors=c("hp","wt","cyl"))
fit2newdata(fit,predictors=c("hp"))
fit2newdata(fit,predictors=c("hp","wt"))
fit=loess(mpg~hp*wt*am,data=mtcars)
fit2newdata(fit,predictors=c("hp"))



cleanEx()
nameEx("getMeans")
### * getMeans

flush(stderr()); flush(stdout())

### Name: getMeans
### Title: calculate mean values of two consecutive number
### Aliases: getMeans

### ** Examples

x=c(50,60,70)
getMeans(x)



cleanEx()
nameEx("getNewFormula")
### * getNewFormula

flush(stderr()); flush(stdout())

### Name: getNewFormula
### Title: Make new formula
### Aliases: getNewFormula

### ** Examples

fit=lm(mpg~factor(cyl)*factor(am)+wt+carb,data=mtcars)
getNewFormula(fit,predictors=c("cyl","wt"))
fit=lm(Sepal.Length~Sepal.Width*Petal.Length+Species,data=iris)
getNewFormula(fit,predictors=c("Petal.Length"))
fit=lm(mpg~hp*wt*factor(cyl),data=mtcars)
getNewFormula(fit,predictors=c("hp","cyl"))
fit=loess(mpg~hp*wt,data=mtcars)
getNewFormula(fit,predictors=c("hp","wt"))



cleanEx()
nameEx("ggPredict")
### * ggPredict

flush(stderr()); flush(stdout())

### Name: ggPredict
### Title: Visualize predictions from the multiple regression models.
### Aliases: ggPredict

### ** Examples

fit=loess(mpg~hp*wt*am,data=mtcars)
ggPredict(fit)
ggPredict(fit,hp)



cleanEx()
nameEx("number2group")
### * number2group

flush(stderr()); flush(stdout())

### Name: number2group
### Title: Convert a numeric vector into groups
### Aliases: number2group

### ** Examples

number2group(iris$Sepal.Length,label="Sepal.Length")
x=number2group(mtcars$wt,label="wt")
x



cleanEx()
nameEx("predict3d")
### * predict3d

flush(stderr()); flush(stdout())

### Name: predict3d
### Title: Draw 3d predict plot using package 'rgl'
### Aliases: predict3d

### ** Examples

fit=lm(mpg~hp*wt,data=mtcars)
predict3d(fit,show.error=TRUE)
fit=lm(log(mpg)~hp*wt,data=mtcars)
predict3d(fit,dep=mpg)



cleanEx()
nameEx("rank2colors")
### * rank2colors

flush(stderr()); flush(stdout())

### Name: rank2colors
### Title: Rank a numeric vector using proportional table and returns
###   character vector of names of color using palette
### Aliases: rank2colors

### ** Examples

rank2colors(mtcars$wt,palette="Blues")



cleanEx()
nameEx("restoreData")
### * restoreData

flush(stderr()); flush(stdout())

### Name: restoreData
### Title: Restore factors in data.frame as numeric
### Aliases: restoreData

### ** Examples

fit=lm(mpg~factor(cyl)*factor(am),data=mtcars)
fit=lm(mpg~wt*factor(am),data=mtcars)
fit=lm(mpg~wt*hp,data=mtcars)
restoreData(fit$model)



cleanEx()
nameEx("restoreData2")
### * restoreData2

flush(stderr()); flush(stdout())

### Name: restoreData2
### Title: restore data column with I() function
### Aliases: restoreData2

### ** Examples

fit=lm(mpg~I(cyl^(1/2))*am,data=mtcars)
restoreData2(fit$model)
fit=lm(mpg~sqrt(hp)*log(wt)*am,data=mtcars)
restoreData2(fit$model)



cleanEx()
nameEx("restoreData3")
### * restoreData3

flush(stderr()); flush(stdout())

### Name: restoreData3
### Title: Restore data from arithmetic operator
### Aliases: restoreData3

### ** Examples

fit=lm(2^mpg~hp*wt,data=mtcars)
summary(fit)
restoreData3(fit$model)



cleanEx()
nameEx("restoreNames")
### * restoreNames

flush(stderr()); flush(stdout())

### Name: restoreNames
### Title: Restore factors in variable name as numeric
### Aliases: restoreNames

### ** Examples

restoreNames(c("factor(cyl)","am"))
restoreNames(c("I(age^2)","am","100/mpg","cyl^1/2","mpg2","sex + 0.5"))



cleanEx()
nameEx("string2pattern")
### * string2pattern

flush(stderr()); flush(stdout())

### Name: string2pattern
### Title: change string to pattern
### Aliases: string2pattern

### ** Examples

string=c("I(age^2)","factor(cyl)","log(mpg)")
string2pattern(string)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
