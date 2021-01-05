pkgname <- "ggiraphExtra"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ggiraphExtra')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("getMapping")
### * getMapping

flush(stderr()); flush(stdout())

### Name: getMapping
### Title: extract variable name from mapping, aes
### Aliases: getMapping

### ** Examples

require(ggplot2)
mapping=aes(colour=sex)
mapping=aes(x=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
getMapping(mapping,"colour")
getMapping(mapping,"x")



cleanEx()
nameEx("ggAncova")
### * ggAncova

flush(stderr()); flush(stdout())

### Name: ggAncova
### Title: Make an interactive plot for an ANCOVA model
### Aliases: ggAncova ggAncova.default ggAncova.formula ggAncova.lm

### ** Examples

require(moonBook)
require(ggplot2)
require(ggiraph)
ggAncova(radial,aes(age,NTAV,color=sex),interactive=TRUE)
fit=lm(NTAV~age+HBP,data=radial)
ggAncova(fit,interactive=TRUE)
ggAncova(NTAV~age+DM,data=radial)



cleanEx()
nameEx("ggArea")
### * ggArea

flush(stderr()); flush(stdout())

### Name: ggArea
### Title: Draw an interactive area plot
### Aliases: ggArea

### ** Examples

require(gcookbook)
require(ggplot2)
ggArea(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))
ggArea(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup),position="fill")



cleanEx()
nameEx("ggBar")
### * ggBar

flush(stderr()); flush(stdout())

### Name: ggBar
### Title: Draw an interactive barplot
### Aliases: ggBar

### ** Examples

require(moonBook)
require(ggplot2)
require(ggiraph)
require(plyr)
ggBar(acs,aes(x=Dx,fill=smoking),interactive=TRUE,width=1,colour="white",size=0.2,polar=TRUE)
ggBar(acs,aes(x=Dx,fill=smoking),position="fill",addlabel=TRUE,horizontal=TRUE,width=0.5)
ggBar(acs,aes(x=Dx,fill=smoking),position="dodge",interactive=TRUE,addlabel=TRUE)
ggBar(acs,aes(x=Dx,fill=smoking),position="fill",addlabel=TRUE)
ggBar(rose,aes(x=Month,fill=group,y=value),stat="identity",polar=TRUE,palette="Reds",width=1,
      color="black",size=0.1,reverse=TRUE,interactive=TRUE)



cleanEx()
nameEx("ggBoxplot")
### * ggBoxplot

flush(stderr()); flush(stdout())

### Name: ggBoxplot
### Title: Draw boxplots of a data.frame
### Aliases: ggBoxplot

### ** Examples

require(ggplot2)
require(ggiraph)
require(reshape2)
ggBoxplot(mtcars,rescale=TRUE)
ggBoxplot(mtcars,aes(x=c(mpg,cyl,disp,hp,drat),color=am),rescale=TRUE)
ggBoxplot(mtcars,aes(x=c(mpg,cyl,disp,hp,drat)),rescale=TRUE)
ggBoxplot(mtcars,rescale=TRUE,interactive=TRUE)
ggBoxplot(mtcars,horizontal=TRUE,interactive=TRUE)



cleanEx()
nameEx("ggCLE")
### * ggCLE

flush(stderr()); flush(stdout())

### Name: ggCLE
### Title: Draw a cleveland dot plot
### Aliases: ggCLE

### ** Examples

require(ggplot2)
require(ggiraph)
ggCLE(data=mtcars,aes(x=mpg),decreasing=FALSE,interactive=TRUE)
ggCLE(data=mtcars,aes(x=mpg,color=am,facet=am),interactive=TRUE)
if(requireNamespace("gcookbook",quietly=TRUE)){
   require(gcookbook)
   ggCLE(data=tophitters2001,aes(x=avg,y=name,color=lg,facet=lg),no=30,interactive=TRUE)
}



cleanEx()
nameEx("ggCatepillar")
### * ggCatepillar

flush(stderr()); flush(stdout())

### Name: ggCatepillar
### Title: Make an interactive catepillar plot
### Aliases: ggCatepillar

### ** Examples

require(moonBook)
require(ggiraph)
require(ggplot2)
ggCatepillar(acs,aes(Dx,age,color=HBP))
ggCatepillar(acs,aes(c(Dx,sex),age,color=HBP),interactive=TRUE,flip=TRUE,use.labels=FALSE)
ggCatepillar(acs,aes(age,height,color=sex),errorbar=FALSE,interactive=TRUE)



cleanEx()
nameEx("ggChoropleth")
### * ggChoropleth

flush(stderr()); flush(stdout())

### Name: ggChoropleth
### Title: Draw an interactive choropleth map
### Aliases: ggChoropleth

### ** Examples

#crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#require(ggplot2)
#require(ggiraph)
#require(maps)
#require(mapproj)
#require(reshape2)
#require(RColorBrewer)
#states_map <- map_data("state")
#ggChoropleth(crimes,aes(fill=Murder,map_id=state),map=states_map,interactive=TRUE)
#ggChoropleth(crimes,aes(fill=c(Murder,Rape),map_id=state),map=states_map,interactive=TRUE)
#ggChoropleth(crimes,aes(map_id=state),map=states_map,palette="OrRd",interactive=TRUE)



cleanEx()
nameEx("ggCor")
### * ggCor

flush(stderr()); flush(stdout())

### Name: ggCor
### Title: Draw a heatmap of correlation test
### Aliases: ggCor

### ** Examples

require(mycor)
require(ggplot2)
require(ggiraph)
require(ppcor)
ggCor(iris,digits=3,label=3)
ggCor(iris,what=3,digits=3,label=3)
ggCor(iris,label=3,interactive=TRUE)
ggCor(mtcars,interactive=TRUE)
ggCor(mtcars,mode=2,interactive=TRUE)
ggCor(iris,method="pearson",interactive=TRUE)



cleanEx()
nameEx("ggDensity")
### * ggDensity

flush(stderr()); flush(stdout())

### Name: ggDensity
### Title: Make a density plot with histogram
### Aliases: ggDensity

### ** Examples

require(ggplot2)
require(moonBook)
ggDensity(acs,aes(x=age))
ggDensity(acs,aes(x=age,color=sex,fill=sex),addhist=FALSE)
ggDensity(acs,aes(x=age,color=sex,fill=sex))
ggDensity(acs,aes(x=age,fill=sex),addhist=FALSE)
ggDensity(acs,aes(x=age,color=sex))



cleanEx()
nameEx("ggDonut")
### * ggDonut

flush(stderr()); flush(stdout())

### Name: ggDonut
### Title: Draw a Donut plot
### Aliases: ggDonut

### ** Examples

require(ggplot2)
require(ggiraph)
require(plyr)
ggDonut(browsers,aes(donuts=version,count=share))
ggDonut(browsers,aes(donuts=version,count=share),palette="Reds",explode=c(2,4,6),labelposition=0)



cleanEx()
nameEx("ggDot")
### * ggDot

flush(stderr()); flush(stdout())

### Name: ggDot
### Title: Draw a Wilkinson dot plot
### Aliases: ggDot

### ** Examples

require(ggplot2)
if(requireNamespace("gcookbook",quietly=TRUE)){ # for data heightweight
 require(gcookbook)
 ggDot(heightweight,aes(sex,heightIn,fill=sex),boxfill="white",binwidth=0.4)
 ggDot(heightweight,aes(heightIn))
 ggDot(heightweight,aes(x=heightIn,fill=sex))
}
require(moonBook) #for use data radial
ggDot(radial,aes(x=sex,y=height,fill=sex),boxfill="white",position=0,binwidth=1,boxwidth=1)
ggDot(radial,aes(x=height,fill=sex),binwidth=1)
ggDot(acs,aes(x=sex,y=age,color=sex))
ggDot(acs,aes(x=Dx,y=age,color=Dx))



cleanEx()
nameEx("ggEffect")
### * ggEffect

flush(stderr()); flush(stdout())

### Name: ggEffect
### Title: Visualize the effect of interaction between two continuous
###   independent variables on a response variable
### Aliases: ggEffect ggEffect.default ggEffect.formula ggEffect.lm

### ** Examples

require(ggplot2)
require(ggiraph)
ggEffect(mtcars,aes(x=wt,y=mpg,color=hp))
ggEffect(mtcars,aes(x=wt,y=mpg,color=hp),interactive=TRUE)
require(moonBook)
ggEffect(acs,aes(x=height,y=weight,color=smoking))
ggEffect(acs,aes(x=height,y=weight,color=smoking),interactive=TRUE)
require(ggplot2)
require(ggiraph)
require(moonBook)
ggEffect(NTAV~age*smoking,data=radial)
require(moonBook)
require(ggplot2)
require(ggiraph)
fit=lm(age~sex*smoking,data=acs)
ggEffect(fit,interactive=TRUE)
ggEffect(radial,aes(x=age,y=NTAV,color=smoking))
ggEffect(radial,aes(x=age,y=NTAV,color=smoking),interactive=TRUE)



cleanEx()
nameEx("ggErrorBar")
### * ggErrorBar

flush(stderr()); flush(stdout())

### Name: ggErrorBar
### Title: Make an interactive bar plot with error bar
### Aliases: ggErrorBar

### ** Examples

require(ggplot2)
require(ggiraph)
ggErrorBar(mpg,aes(x=drv,y=cty))
ggErrorBar(mpg,aes(x=drv,y=hwy,color=cyl),mode=1,interactive=TRUE,errorbar="sd")



cleanEx()
nameEx("ggHSD")
### * ggHSD

flush(stderr()); flush(stdout())

### Name: ggHSD
### Title: Draw Tukey Honest Significant Differences plot
### Aliases: ggHSD

### ** Examples

require(ggplot2)
fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
result=TukeyHSD(fm1, "tension", ordered = TRUE)
str(result)
ggHSD(result)
ggHSD(result,interactive=TRUE)



cleanEx()
nameEx("ggHeatmap")
### * ggHeatmap

flush(stderr()); flush(stdout())

### Name: ggHeatmap
### Title: Make an interactive Heatmap
### Aliases: ggHeatmap

### ** Examples

require(moonBook)
require(ggplot2)
require(ggiraph)
require(sjmisc)
ggHeatmap(acs,aes(x=Dx,y=smoking),addlabel=TRUE,interactive=TRUE)
ggHeatmap(acs,aes(x=sex,y=Dx,fill=age),addlabel=TRUE,interactive=TRUE)
ggHeatmap(rose,aes(x=Month,y=group,fill=value),stat="identity",addlabel=TRUE)
ggHeatmap(rose,aes(x=Month,y=group,fill=value),addlabel=TRUE)
ggHeatmap(taco,aes(x=AgeGroup,y=Filling,fill=Rating,facet=ShellType),color="grey50",stat="identity")



cleanEx()
nameEx("ggPair")
### * ggPair

flush(stderr()); flush(stdout())

### Name: ggPair
### Title: Make an interactive scatter and line plot
### Aliases: ggPair

### ** Examples

require(ggplot2)
require(ggiraph)
require(sjmisc)
require(moonBook)
ggPair(iris,rescale=TRUE,horizontal=TRUE)
ggPair(acs,aes(colour=smoking),horizontal=TRUE,rescale=TRUE)
ggPair(radial,aes(color=male),horizontal=TRUE,rescale=TRUE)
ggPair(mtcars,horizontal=TRUE,rescale=TRUE)
ggPair(iris,rescale=TRUE,horizontal=TRUE,interactive=TRUE)
ggPair(iris,aes(color=Species),rescale=TRUE,interactive=TRUE)
ggPair(iris,aes(x=c(Sepal.Length,Sepal.Width),color=Species),horizontal=TRUE,interactive=TRUE)



cleanEx()
nameEx("ggPie")
### * ggPie

flush(stderr()); flush(stdout())

### Name: ggPie
### Title: Draw a pie plot
### Aliases: ggPie

### ** Examples

require(ggplot2)
require(ggiraph)
require(plyr)
require(moonBook)
ggPie(data=browsers,aes(pies=browser,count=share))
ggPie(data=acs,aes(pies=Dx))



cleanEx()
nameEx("ggPieDonut")
### * ggPieDonut

flush(stderr()); flush(stdout())

### Name: ggPieDonut
### Title: Draw a Pie and Donut plot
### Aliases: ggPieDonut

### ** Examples

require(ggplot2)
require(ggiraph)
require(plyr)
require(moonBook)
ggPieDonut(acs,aes(pies=Dx,donuts=smoking))
ggPieDonut(acs,aes(pies=smoking))
ggPieDonut(browsers,aes(pies=browser,donuts=version,count=share))
ggPieDonut(browsers,aes(x=c(browser,version),y=share),interactive=TRUE)



cleanEx()
nameEx("ggPoints")
### * ggPoints

flush(stderr()); flush(stdout())

### Name: ggPoints
### Title: Make an interactive scatterplot with regression line(s)
### Aliases: ggPoints

### ** Examples

require(ggplot2)
require(ggiraph)
require(plyr)
ggPoints(aes(x=wt,y=mpg,fill=am),data=mtcars)
ggPoints(aes(x=wt,y=mpg),data=mtcars)
ggPoints(aes(x=wt,y=mpg,fill=am),data=mtcars,method="lm",interactive=TRUE)
ggPoints(aes(x=wt,y=mpg,color=am),data=mtcars,interactive=TRUE)



cleanEx()
nameEx("ggPredict")
### * ggPredict

flush(stderr()); flush(stdout())

### Name: ggPredict
### Title: Visualize predictions from the multiple regression models.
### Aliases: ggPredict

### ** Examples

require(moonBook)
require(ggplot2)
require(ggiraph)
require(plyr)
fit=lm(NTAV~age*weight,data=radial)
fit=lm(NTAV~age*weight*DM,data=radial)
fit=lm(NTAV~age+DM,data=radial)
ggPredict(fit,interactive=TRUE)
require(TH.data)
fit=glm(cens~pnodes*horTh,data=GBSG2,family=binomial)
ggPredict(fit,se=TRUE)
fit1=glm(cens~pnodes*age,data=GBSG2,family=binomial)
ggPredict(fit1)
ggPredict(fit1,colorn=100,jitter=FALSE,interactive=TRUE)
fit2=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
ggPredict(fit2,colorn=100,jitter=FALSE,interactive=TRUE)



cleanEx()
nameEx("ggRadar")
### * ggRadar

flush(stderr()); flush(stdout())

### Name: ggRadar
### Title: Draw a radar chart
### Aliases: ggRadar

### ** Examples

require(ggplot2)
require(ggiraph)
require(plyr)
require(reshape2)
require(moonBook)
require(sjmisc)
ggRadar(data=iris,aes(group=Species))
ggRadar(data=mtcars,interactive=TRUE)
ggRadar(data=mtcars,aes(colour=am,facet=cyl),interactive=TRUE)
ggRadar(data=acs,aes(colour=Dx,facet=Dx))
ggRadar(iris,aes(x=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width)))



cleanEx()
nameEx("ggRose")
### * ggRose

flush(stderr()); flush(stdout())

### Name: ggRose
### Title: Draw an interactive Rose plot
### Aliases: ggRose

### ** Examples

require(moonBook)
require(ggplot2)
require(ggiraph)
require(plyr)
ggRose(rose,aes(x=Month,fill=group,y=value),stat="identity",interactive=TRUE)
ggRose(acs,aes(x=Dx,fill=smoking),interactive=TRUE)



cleanEx()
nameEx("ggSpine")
### * ggSpine

flush(stderr()); flush(stdout())

### Name: ggSpine
### Title: Draw an interactive spinogram
### Aliases: ggSpine

### ** Examples

require(moonBook)
require(ggplot2)
acs$Dx=factor(acs$Dx,levels=c("Unstable Angina","NSTEMI","STEMI"))
ggSpine(data=acs,aes(x=age,fill=Dx,facet=sex),palette="Reds")
ggSpine(data=acs,aes(x=age,fill=Dx,facet=sex),facetbycol=FALSE,minlabelgroup=0.02)
ggSpine(data=acs,aes(x=age,fill=Dx),palette="Reds")
ggSpine(data=acs,aes(x=smoking,fill=Dx),palette="Reds")
ggSpine(data=acs,aes(x=DM,fill=Dx,facet=sex),palette="Reds")
ggSpine(data=acs,aes(x=Dx,fill=smoking,facet=sex),palette="Reds")
ggSpine(data=acs,aes(x=DM,facet=smoking,fill=Dx),sec.y.axis=TRUE)
ggSpine(data=acs,aes(x=DM,facet=smoking,fill=Dx),facetbycol=FALSE)
ggSpine(mtcars,aes(x=gear,fill=carb),interactive=TRUE)
ggSpine(mtcars,aes(x=gear,fill=carb,facet=am))
ggSpine(data=acs,aes(x=Dx,fill=smoking),position="dodge")
ggSpine(data=acs,aes(x=Dx,fill=smoking),position="stack")



cleanEx()
nameEx("ggViolin")
### * ggViolin

flush(stderr()); flush(stdout())

### Name: ggViolin
### Title: Draw violin plots of a data.frame
### Aliases: ggViolin

### ** Examples

require(ggplot2)
require(ggiraph)
require(reshape2)
ggViolin(iris)
ggViolin(iris,aes(fill=Species),rescale=TRUE)
ggViolin(mtcars,aes(x=c(mpg,cyl,disp,hp,drat),color=am),rescale=TRUE)
ggViolin(mtcars,aes(x=c(mpg,cyl,disp,hp,drat)),rescale=TRUE)



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
