% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycsv.R
\name{mycsv.mytable}
\alias{mycsv.mytable}
\title{Export to csv file for class "mytable"}
\usage{
\method{mycsv}{mytable}(x, row.names = FALSE, ...)
}
\arguments{
\item{x}{An object of class "mytable" a result of a call to \code{\link{mytable}}}

\item{row.names}{either a logical value indicating whether the row names of x
are to be written along with x, or a character vector of
row names to be written.}

\item{...}{further arguments passed to or from other methods.}
}
\description{
Export to csv file for class "mytable"
}
\examples{
\dontrun{
require(moonBook)
res=mytable(sex~age+DM,data=acs)
mycsv(res,file="test.csv")
mycsv(summary(res),file="testsummary.csv")
}
}
