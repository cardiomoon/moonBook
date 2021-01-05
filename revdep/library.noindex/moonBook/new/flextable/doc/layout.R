## ----echo = FALSE, message=FALSE----------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  collapse = TRUE,
  comment = "#>", 
  eval = TRUE
)
library(htmltools)
library(magrittr)
library(data.table)

as_ul <- function( names ) {
  do.call( tags$ul, lapply(names, tags$li) )
}


## -----------------------------------------------------------------------------
library(flextable)
library(officer)

## -----------------------------------------------------------------------------
data <- iris[c(1:3, 51:53, 101:104),]
myft <- flextable(data, col_keys = c("Species", "Sepal.Length", "Petal.Length") )
myft

## -----------------------------------------------------------------------------
myft <- flextable(
  data = data, 
  col_keys = c("Species", "col_1", "Sepal.Length", "Petal.Length") )
myft <- theme_vanilla(myft)
myft <- autofit(myft)
myft <- empty_blanks(myft)
myft

## -----------------------------------------------------------------------------
ft <- flextable( head( iris ) ) 
ft <- set_header_labels(ft, Sepal.Length = "Sepal length", 
    Sepal.Width = "Sepal width", Petal.Length = "Petal length",
    Petal.Width = "Petal width" )
ft

## -----------------------------------------------------------------------------

dat <- data.frame(
  letters1 = c("a", "b", "b", "c"), 
  letters2 = c("d", "e", "b", "b"), 
  number = 1:4, stringsAsFactors = FALSE )

myft <- flextable(dat)
myft <- theme_box(myft)
myft

## -----------------------------------------------------------------------------
merge_v(myft, j = ~ letters1 + letters2 )

## -----------------------------------------------------------------------------
merge_h(myft)

## -----------------------------------------------------------------------------
merge_h_range(myft, 
  i =  ~ number < 3, 
  j1 = "letters1", j2 = "letters2")

## -----------------------------------------------------------------------------
myft %>% 
  merge_at(
    i = 1:2, j = 1:2)

## -----------------------------------------------------------------------------
merge_none(myft)

## -----------------------------------------------------------------------------
ft <- data.frame(a = 1:5, b = 6:10) %>%
  flextable() %>% theme_box() %>%
  merge_at(i = 4:5, j = 1, part = "body") %>%
  hline(i = 5, part = "body",
        border = fp_border(color = "orange", width = 3) )
ft
fix_border_issues(ft)

## -----------------------------------------------------------------------------
ft <- flextable( head( iris ) ) 
ft <- set_header_labels(ft, Sepal.Length = "Sepal", 
    Sepal.Width = "Sepal", Petal.Length = "Petal",
    Petal.Width = "Petal" )
# merge them 
ft <- merge_at(ft, i = 1, j = 1:2, part = "header")
ft <- merge_at(ft, i = 1, j = 3:4, part = "header")
ft

## -----------------------------------------------------------------------------
ft <- add_header_row(ft, 
  values = c("", "length", "width", "length", "width"), top = FALSE )
ft <- theme_box(ft)

## ----echo=FALSE---------------------------------------------------------------
ft

## -----------------------------------------------------------------------------
ft <- add_header_lines(ft, 
  values = c("this is a first line", 
     "this is a second line") ) 
ft <- theme_box(ft)

## ----echo=FALSE---------------------------------------------------------------
ft

## -----------------------------------------------------------------------------
typology <- data.frame(
  col_keys = c( "Sepal.Length", 
    "Sepal.Width", "Petal.Length",
    "Petal.Width", "Species" ),
  type = c("double", "double", "double", 
    "double", "factor"),
  what = c("Sepal", "Sepal", "Petal", 
    "Petal", "Species"),
  measure = c("Length", "Width", "Length", 
    "Width", "Species"),
  stringsAsFactors = FALSE )

## ----echo=FALSE---------------------------------------------------------------
autofit( theme_vanilla(flextable(typology)) )

## -----------------------------------------------------------------------------
ft <- flextable( head( iris ) )
ft <- set_header_df( ft, mapping = typology, key = "col_keys" )

ft <- merge_h(ft, part = "header")
ft <- merge_v(ft, part = "header")

ft <- theme_booktabs(ft)
ft <- autofit(ft)
ft <- fix_border_issues(ft)

## ----echo=FALSE---------------------------------------------------------------
ft

## -----------------------------------------------------------------------------
ft <- qflextable(head(airquality))
set_table_properties(ft, width = .5, layout = "autofit")
set_table_properties(ft, width = 1, layout = "autofit")

## -----------------------------------------------------------------------------
ft_base <- flextable(head(mtcars))
ft_base <- theme_vader(ft_base)
ft_base
dim(ft_base)

## -----------------------------------------------------------------------------
dim_pretty(ft_base)

## -----------------------------------------------------------------------------
ft <- autofit(ft_base, add_w = 0, add_h = 0)

dim(ft)
ft

## -----------------------------------------------------------------------------
ft <- hrule(ft, rule = "exact")
ft <- valign(ft, valign = "top")
ft <- width(ft, width = .5)
ft <- width(ft, j = ~ mpg + disp, width = 1.5)
ft <- height_all( ft, height = .6 )
ft <- height( ft, i = 3, height = 1 )
ft

