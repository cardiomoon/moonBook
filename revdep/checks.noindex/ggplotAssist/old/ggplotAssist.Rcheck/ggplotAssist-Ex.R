pkgname <- "ggplotAssist"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ggplotAssist')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ggplotAssist")
### * ggplotAssist

flush(stderr()); flush(stdout())

### Name: ggplotAssist
### Title: A shiny app for learn ggplot2
### Aliases: ggplotAssist

### ** Examples

library(tidyverse)
library(rstudioapi)
library(miniUI)
library(moonBook)
library(shinyAce)
library(ggthemes)
library(shiny)
library(stringr)
library(editData)
library(shinyWidgets)
library(gcookbook)
library(shiny)
# Only run examples in interactive R sessions
if (interactive()) {
    result<-ggplotAssist(mtcars)
    result
}



cleanEx()
nameEx("selectizeInput3")
### * selectizeInput3

flush(stderr()); flush(stdout())

### Name: selectizeInput3
### Title: side-by-side selectizeInput
### Aliases: selectizeInput3

### ** Examples

library(shiny)
# Only run examples in interactive R sessions
if (interactive()) {
  ui <- fluidPage(
         selectizeInput3("color", "color", choices=colors())
    )
    server <- function(input, output) {

    }
    shinyApp(ui, server)
}



cleanEx()
nameEx("textAreaInput4")
### * textAreaInput4

flush(stderr()); flush(stdout())

### Name: textAreaInput4
### Title: Create side-by side textAreaInput with disabled spell check
### Aliases: textAreaInput4

### ** Examples

library(shiny)
# Only run examples in interactive R sessions
if (interactive()) {
     ui <- fluidPage(
          textAreaInput4("Code","Code","")
     )
     server <- function(input, output) {
          
     }
     shinyApp(ui, server)
}  



cleanEx()
nameEx("textFunctionInput")
### * textFunctionInput

flush(stderr()); flush(stdout())

### Name: textFunctionInput
### Title: UI of textFunction shiny module
### Aliases: textFunctionInput

### ** Examples

library(ggplotAssist)
library(shiny)
# Only run examples in interactive R sessions
if(interactive()){
ui=fluidPage(
   textFunctionInput("text"),
   textOutput("text")
)
server=function(input,output,session){
   rv=reactiveValues()
   rawData=read.csv("data-raw/setting.csv",stringsAsFactors = FALSE)
   settingData=splitData(rawData,"setting")
   rv$argList<-list(label="text",mode="text",value="element_text()",choices=NULL,width=200,
                    bg="lightcyan",placeholder="")
   result=callModule(textFunction,"text",argList=reactive(rv$argList),
                     editCode=reactive(TRUE),settingData=reactive(settingData))
   output$text=renderText({
       result()
   })
}
shinyApp(ui,server)
}



cleanEx()
nameEx("textInput4")
### * textInput4

flush(stderr()); flush(stdout())

### Name: textInput4
### Title: Create side-by side textInput with disabled spell check
### Aliases: textInput4

### ** Examples

library(shiny)
# Only run examples in interactive R sessions
if (interactive()) {
     ui <- fluidPage(
          textInput4("id", "id", ""),
          textInput4("name","name","")
     )
     server <- function(input, output) {
          
     }
     shinyApp(ui, server)
}  



cleanEx()
nameEx("uiOutput3")
### * uiOutput3

flush(stderr()); flush(stdout())

### Name: uiOutput3
### Title: Create side-by side uiOutput
### Aliases: uiOutput3

### ** Examples

library(shiny)
# Only run examples in interactive R sessions
if (interactive()) {
     ui <- fluidPage(
          textInput4("name","name",""),
          uiOutput3("test")
     )
     server <- function(input, output) {
          
     }
     shinyApp(ui, server)
}  



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
