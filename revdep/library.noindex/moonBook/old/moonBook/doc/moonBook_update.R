## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  message =FALSE
)

## ----comment=NA----------------------------------------------------------
require(moonBook)
require(ztable)
require(magrittr)
options(ztable.type="html")

mytable(acs)

## ----comment=NA----------------------------------------------------------
mytable(~age+sex,data=acs)

## ------------------------------------------------------------------------
mytable(Dx~sex,data=acs)

## ------------------------------------------------------------------------
mytable(Dx~sex,data=acs) %>% compress

## ------------------------------------------------------------------------
mytable(Dx~sex,data=acs) %>% compress(no=1)

## ------------------------------------------------------------------------
mytable(Dx~cardiogenicShock+DM+obesity+HBP,data=acs) %>% compress

## ------------------------------------------------------------------------
mytable(Dx~cardiogenicShock+DM+obesity+HBP,data=acs) %>% compress(add.label=FALSE)

## ----results='asis'------------------------------------------------------
mytable(Dx~cardiogenicShock+DM+obesity+HBP,data=acs) %>% compress(add.label=FALSE) %>% ztable

## ------------------------------------------------------------------------
mytable(sex~Dx,data=acs)

## ------------------------------------------------------------------------
mytable(sex~Dx,data=acs) %>% deleteRows(2)

## ------------------------------------------------------------------------
mytable(sex+HBP~age+Dx,data=acs) %>% deleteRows(3)

## ------------------------------------------------------------------------
mytable(obesity~HBP,data=acs,catMethod=1,show.all=TRUE)

## ------------------------------------------------------------------------
data(diamonds,package="ggplot2")
mytable(diamonds) %>% addComma

## ----results='asis'------------------------------------------------------
mytable(cut~.,data=diamonds) %>% addComma %>% ztable

