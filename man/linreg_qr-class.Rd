% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qr_decomposition.r
\docType{class}
\name{linreg_qr-class}
\alias{linreg_qr-class}
\alias{linreg_qr}
\title{linreg RC Class Object using QR Decomposition}
\arguments{
\item{formula}{formula of regression model}

\item{data}{data of regression model}
}
\description{
linreg RC Class Object using QR Decomposition
}

\examples{
data("iris")
linreg_mod <- linreg_qr$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data = iris)
linreg_mod$print()
}
