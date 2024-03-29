% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ORplot.R
\name{extractOR}
\alias{extractOR}
\title{Extract the odds ratios from a S3 object of glm}
\usage{
extractOR(x, digits = 2, method = "default")
}
\arguments{
\item{x}{A S3 object of glm}

\item{digits}{An integer indicating the number of decimal places (round) or
significant digits to be used. Default value is 2.}

\item{method}{Method to compute confidence interval. Choices are one of c("default","LRT").}
}
\value{
A data.frame consist of odds ratios and 95% confidence interval and
        p values
}
\description{
Extract the odds ratios from a S3 object of glm
}
\examples{
data(cancer,package="survival")
x=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
extractOR(x)
}
