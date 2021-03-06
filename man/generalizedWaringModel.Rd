% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcmdrPlugin.GWRM.r
\name{generalizedWaringModel}
\alias{generalizedWaringModel}
\title{Rcmdr Generalized Waring Regression Model Dialog}
\usage{
generalizedWaringModel()
}
\value{
It returns a new \code{GW} model in the model list.
}
\description{
This dialog is used to specify a generalized Waring regression model to be fit by the \code{gw} function
}
\details{
It shares a common general structure with that of the Linear Model (see \code{\link{linearModel}}). Therefore, the use of this dialog box is similar 
to the linear model except the box labelled \code{Model parameters}, in which a fixed value for 
the parameter \code{k} can be specificied; if it is not supplied, the \code{k} estimate is computed.
}
\examples{
library(RcmdrPlugin.GWRM)

}
