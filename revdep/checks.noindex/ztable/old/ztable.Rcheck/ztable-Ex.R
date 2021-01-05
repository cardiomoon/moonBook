pkgname <- "ztable"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ztable')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addCellColor")
### * addCellColor

flush(stderr()); flush(stdout())

### Name: addCellColor
### Title: Add column colors of an object of ztable
### Aliases: addCellColor

### ** Examples

## Not run: 
##D z=ztable(head(iris))
##D z=addRowColor(z,c(1,3),color="platinum")
##D z=addColColor(z,2,color="cyan")
##D z=addCellColor(z,cols=c(5,4),rows=5,color="red")
##D z
## End(Not run)



cleanEx()
nameEx("addColColor")
### * addColColor

flush(stderr()); flush(stdout())

### Name: addColColor
### Title: Add column colors of an object of ztable
### Aliases: addColColor

### ** Examples

z=ztable(head(iris))
z=addColColor(z,c(1,3),color="platinum")
z



cleanEx()
nameEx("addFrontColor")
### * addFrontColor

flush(stderr()); flush(stdout())

### Name: addFrontColor
### Title: Add column colors of an object of ztable
### Aliases: addFrontColor

### ** Examples

z=ztable(head(iris))
z=addFrontColor(z,rows=2:4,cols=c(2,4,6),color=c("red","green","blue"))
z



cleanEx()
nameEx("addRowColor")
### * addRowColor

flush(stderr()); flush(stdout())

### Name: addRowColor
### Title: Add row colors of an object of ztable
### Aliases: addRowColor

### ** Examples

z=ztable(head(iris))
z=addRowColor(z,c(1,3),color="platinum")
z



cleanEx()
nameEx("color2hex")
### * color2hex

flush(stderr()); flush(stdout())

### Name: color2hex
### Title: Convert a named color into a hexadecimal color with rgb value
### Aliases: color2hex

### ** Examples

color2hex("green")
color2hex("red")



cleanEx()
nameEx("makeHeatmap")
### * makeHeatmap

flush(stderr()); flush(stdout())

### Name: makeHeatmap
### Title: Add gradient background color to ztable
### Aliases: makeHeatmap

### ** Examples

require(magrittr)
ztable(head(mtcars)) %>% makeHeatmap()
## Not run: 
##D ztable(head(mtcars)) %>% makeHeatmap(palette="YlOrRd",cols=c(1,4,6),margin=2)
##D ztable(head(mtcars)) %>% makeHeatmap(rows=c(1,3,5),margin=1)
##D require(moonBook)
##D x=table(acs$smoking,acs$Dx)
##D ztable(x) %>% makeHeatmap
##D ztable(x) %>% makeHeatmap(palette="Blues")
##D ztable(x) %>% makeHeatmap(mycolor=gradientColor(low="yellow",mid="orange",high="red"))
## End(Not run)



cleanEx()
nameEx("palette2colors")
### * palette2colors

flush(stderr()); flush(stdout())

### Name: palette2colors
### Title: Extract hexadecimal colors from a color palette
### Aliases: palette2colors

### ** Examples

require(RColorBrewer)
require(magrittr)
palette2colors("Reds")
ztable(head(mtcars,10)) %>%
     addColColor(cols=1:12,bg=palette2colors("Set3"))



cleanEx()
nameEx("parallelTables")
### * parallelTables

flush(stderr()); flush(stdout())

### Name: parallelTables
### Title: Place two or more ztables or figures side by side in Latex or
###   HTML format
### Aliases: parallelTables

### ** Examples

require(ztable)
z=ztable(head(mtcars[1:3]),tabular=TRUE)
parallelTables(c(0.4,0.3),list(z,z))
parallelTables(c(0.5,0.5),list(z,z))
parallelTables(c(0.5,0.5),list(z,z,type="html"))
z1=ztable(head(iris[1:3]),turn=TRUE,angle=10,zebra=1)
z2=ztable(head(iris[1:3]),turn=TRUE,angle=-10,zebra=2)
parallelTables(c(0.5,0.5),list(z1,z2))



cleanEx()
nameEx("totalLeft")
### * totalLeft

flush(stderr()); flush(stdout())

### Name: totalLeft
### Title: Arrange total column to the left
### Aliases: totalLeft

### ** Examples

require(moonBook)
require(ztable)
require(magrittr)
mytable(sex~.,data=acs,show.total=TRUE) %>% ztable() %>% totalLeft()
## Not run: 
##D mytable(sex+Dx~.,data=acs,show.total=TRUE) %>% ztable %>% totalLeft
## End(Not run)



cleanEx()
nameEx("ztable.cbind.mytable")
### * ztable.cbind.mytable

flush(stderr()); flush(stdout())

### Name: ztable.cbind.mytable
### Title: Make ztable from object cbind.mytable
### Aliases: ztable.cbind.mytable

### ** Examples

require(moonBook)
res=mytable(sex+DM~.,data=acs)
z=ztable(res)
z



cleanEx()
nameEx("ztable.mytable")
### * ztable.mytable

flush(stderr()); flush(stdout())

### Name: ztable.mytable
### Title: Make ztable from object mytable
### Aliases: ztable.mytable

### ** Examples

require(moonBook)
res=mytable(sex~.,data=acs)
z=ztable(res)
z



cleanEx()
nameEx("ztable2flextable")
### * ztable2flextable

flush(stderr()); flush(stdout())

### Name: ztable2flextable
### Title: Convert an object of ztable into an object of flextable
### Aliases: ztable2flextable

### ** Examples

z=ztable(head(mtcars))
ztable2flextable(z)



cleanEx()
nameEx("ztable_sub")
### * ztable_sub

flush(stderr()); flush(stdout())

### Name: ztable_sub
### Title: Exporting "data.frame" to an object of class "ztable"
### Aliases: ztable_sub

### ** Examples

require(ztable)
x=head(iris)
ztable(x)
## Not run: 
##D ztable(x,size=3,caption="Table 1. ztable Test")
##D ztable(x,size=7,caption="Table 1. ztable Test",caption.position="l")
##D ztable(x,size=7,caption="Table 1. ztable Test",caption.placement="bottom",
##D       caption.position="l")
##D fit=lm(mpg~.,data=mtcars)
##D ztable(fit)
##D data(USArrests)
##D pr1 <- prcomp(USArrests)
##D ztable(pr1)
##D ztable(summary(pr1))
##D require(survival)
##D data(colon)
##D attach(colon)
##D out <- glm(status ~ rx+obstruct+adhere+nodes+extent, data=colon, family=binomial)
##D ztable(out)
##D colon$TS = Surv(time,status==1)
##D out1=coxph(TS~rx+obstruct+adhere+differ+extent+surg+node4,data=colon)
##D ztable(out1)
##D ztable(head(mtcars),zebra=1)
##D ztable(head(mtcars),zebra=1,zebra.type=2)
## End(Not run)



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
