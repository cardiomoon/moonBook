pkgname <- "webr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('webr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PieDonut")
### * PieDonut

flush(stderr()); flush(stdout())

### Name: PieDonut
### Title: Draw a PieDonut plot
### Aliases: PieDonut

### ** Examples

require(moonBook)
require(ggplot2)
browser=c("MSIE","Firefox","Chrome","Safari","Opera")
share=c(50,21.9,10.8,6.5,1.8)
df=data.frame(browser,share)
PieDonut(df,aes(browser,count=share),r0=0.7,start=3*pi/2,labelpositionThreshold=0.1)



cleanEx()
nameEx("cox.stuart.test")
### * cox.stuart.test

flush(stderr()); flush(stdout())

### Name: cox.stuart.test
### Title: Cox-Stuart test for trend analysis The Cox-Stuart test is
###   defined as a little powerful test (power equal to 0.78), but very
###   robust for the trend analysis. It is therefore applicable to a wide
###   variety of situations, to get an idea of the evolution of values
###   obtained. The proposed method is based on the binomial distribution.
###   This function was written by Tommaso Martino<todoslogos@gmail.com>
###   (See 'References')
### Aliases: cox.stuart.test

### ** Examples

customers = c(5, 9, 12, 18, 17, 16, 19, 20, 4, 3, 18, 16, 17, 15, 14)
cox.stuart.test(customers)



cleanEx()
nameEx("freqSummary")
### * freqSummary

flush(stderr()); flush(stdout())

### Name: freqSummary
### Title: Make table summarizing frequency
### Aliases: freqSummary

### ** Examples

require(moonBook)
freqSummary(acs$Dx)
#freqSummary(acs$smoking,lang="kor")



cleanEx()
nameEx("freqTable")
### * freqTable

flush(stderr()); flush(stdout())

### Name: freqTable
### Title: Make flextable summarizing frequency
### Aliases: freqTable

### ** Examples

require(moonBook)
freqTable(acs$Dx)
#freqTable(acs$smoking,lang="kor",vanilla=TRUE,fontsize=12)



cleanEx()
nameEx("numSummary")
### * numSummary

flush(stderr()); flush(stdout())

### Name: numSummary
### Title: Numerical Summary
### Aliases: numSummary numSummary1 numSummary2

### ** Examples

require(moonBook)
require(magrittr)
require(dplyr)
require(rrtable)
require(webr)
require(tibble)
numSummary(acs)
numSummary(acs$age)
numSummary(acs,age,EF)
acs %>% group_by(sex) %>% numSummary(age,BMI)
acs %>% group_by(sex) %>% select(age) %>% numSummary
acs %>% group_by(sex) %>% select(age,EF) %>% numSummary
acs %>% group_by(sex,Dx) %>% select(age,EF) %>% numSummary
acs %>% group_by(sex,Dx) %>% select(age) %>% numSummary
#acs %>% group_by(sex,Dx) %>% numSummary(age,EF,lang="kor")



cleanEx()
nameEx("numSummaryTable")
### * numSummaryTable

flush(stderr()); flush(stdout())

### Name: numSummaryTable
### Title: Make a table showing numerical summary
### Aliases: numSummaryTable

### ** Examples

require(moonBook)
require(dplyr)
numSummaryTable(acs)
numSummaryTable(acs$age)
acs %>% group_by(sex) %>% select(age) %>% numSummaryTable
acs %>% group_by(sex) %>% select(age,EF) %>% numSummaryTable
acs %>% group_by(sex,Dx) %>% select(age,EF) %>% numSummaryTable(vanilla=FALSE)
acs %>% group_by(sex,Dx) %>% numSummaryTable(age,EF,add.rownames=FALSE)



cleanEx()
nameEx("plot.htest")
### * plot.htest

flush(stderr()); flush(stdout())

### Name: plot.htest
### Title: Plotting distribution of statistic for object "htest"
### Aliases: plot.htest

### ** Examples


require(moonBook)
require(webr)
## chi-square test
x=chisq.test(table(mtcars$am,mtcars$cyl))
plot(x)

#Welch Two Sample t-test
x=t.test(mpg~am,data=mtcars)
plot(x)



cleanEx()
nameEx("runs.test")
### * runs.test

flush(stderr()); flush(stdout())

### Name: runs.test
### Title: Runs test for randomness
### Aliases: runs.test

### ** Examples

y=c(1,2,2,1,1,2,1,2)
runs.test(y)
y=c("A","B","B","A","A","B","A","B")
runs.test(y,alternative="p")



cleanEx()
nameEx("x2Table")
### * x2Table

flush(stderr()); flush(stdout())

### Name: x2Table
### Title: Make a chisquare result table
### Aliases: x2Table

### ** Examples

require(moonBook)
x2Table(acs,sex,DM)



cleanEx()
nameEx("x2summary")
### * x2summary

flush(stderr()); flush(stdout())

### Name: x2summary
### Title: Summarize chisquare result
### Aliases: x2summary

### ** Examples

require(moonBook)
x2summary(acs,sex,DM)



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
