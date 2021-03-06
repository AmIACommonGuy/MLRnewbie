% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LR_func.R
\name{ssmodel}
\alias{ssmodel}
\title{Simple Anova Table}
\usage{
ssmodel(model)
}
\arguments{
\item{model}{A list object returned by linearRegression.}
}
\value{
A list of the three sum of squares: Residual sum of square,
Regression sum of square, and total sum of square.
}
\description{
Compute analysis of variance (or deviance) tables
for a single fitted model objects returned by linearRegression function.
}
\examples{
mt <- mtcars
ssmodel(linearRegression(mt, "mpg", "cyl", TRUE))
}
