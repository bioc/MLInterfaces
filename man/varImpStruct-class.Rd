\name{varImpStruct-class}
\docType{class}
\alias{varImpStruct-class}
\alias{plot,varImpStruct-method}
\alias{show,varImpStruct-method}
\alias{getVarImp}
\alias{getVarImp,classifOutput-method}
\title{Class "varImpStruct" -- collect data on variable importance
 from various machine learning methods}
\description{  collects data on variable importance }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("varImpStruct", ...)}.
These are matrices of importance measures with separate 
slots identifying algorithm generating the measures and
variable names.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"} actual importance
measures }
    \item{\code{method}:}{Object of class \code{"character"} tag }
    \item{\code{varnames}:}{Object of class \code{"character"} conformant
vector of names of variables }
  }
}
\section{Extends}{
Class \code{"matrix"}, from data part.
Class \code{"structure"}, by class \code{"matrix"}.
Class \code{"array"}, by class \code{"matrix"}.
Class \code{"vector"}, by class "matrix", with explicit coerce.
Class \code{"vector"}, by class "matrix", with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "varImpStruct")}: make a bar plot,
you can supply an argument resolveenv which will look up variable
names in a translation environment }
    \item{show}{\code{signature(object = "varImpStruct")}: simple abbreviated
display }
    \item{getVarImp}{\code{signature(object = "classifOutput")}: extractor
of variable importance structure}
  }
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%  or \code{\link{CLASSNAME-class}} for links to other classes
%}
\examples{
library(golubEsets)
data(golubMerge)
library(hu6800)
smallG <- golubMerge[1:60,]
rf <- randomForestB( smallG, "ALL.AML", 1:40, importance=TRUE )
plot( getVarImp( rf ), n=10, resolveenv=hu6800SYMBOL )
gb <- gbmB( smallG, "ALL.AML", 1:40 , n.minobsinnode=3 , n.trees=6000)
plot( getVarImp( gb ), n=10, resolveenv=hu6800SYMBOL )
}
\keyword{classes}