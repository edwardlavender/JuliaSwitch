% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JuliaConnectoR.R
\name{JuliaConnectoR-wrappers}
\alias{JuliaConnectoR-wrappers}
\alias{juliaAllot}
\alias{juliaAllot.default}
\alias{juliaAllot.data.frame}
\alias{juliaAllot.SpatRaster}
\alias{juliaClass}
\alias{juliaTranslate}
\alias{juliaTranslate.default}
\alias{juliaTranslate.DataFrame}
\title{\code{JuliaConnectoR} wrappers}
\usage{
juliaAllot(name, value)

\method{juliaAllot}{default}(name, value)

\method{juliaAllot}{data.frame}(name, value)

\method{juliaAllot}{SpatRaster}(name, value)

juliaClass(x)

juliaTranslate(x)

\method{juliaTranslate}{default}(x)

\method{juliaTranslate}{DataFrame}(x)
}
\arguments{
\item{name, value}{Arguments for \code{\link[=juliaAllot]{juliaAllot()}}.
\itemize{
\item \code{name} is a \code{character} that defines the object name in \code{Julia}.
\item \code{value} is the \code{R} object.
}}

\item{x}{Arguments for \code{\link[=juliaClass]{juliaClass()}} and \code{\link[=juliaTranslate]{juliaTranslate()}}.
\itemize{
\item \code{x} is a \code{character} that defines the name of an object in \code{Julia}.
}}
}
\description{
These internal functions wrap \code{JuliaConnectoR} routines.
}
\details{
\itemize{
\item \code{\link[=juliaAllot]{juliaAllot()}} is a \code{\link[=julia_allot]{julia_allot()}} equivalent.
\itemize{
\item The default method wraps \code{\link[JuliaConnectoR:juliaCall]{JuliaConnectoR::juliaCall()}};
\item The \code{data.frame} method translates \code{data.frame} inputs to \code{DataFrame}s;
\item The \code{SpatRaster} method translates \code{\link[terra:SpatRaster-class]{terra::SpatRaster}}s to \code{GeoArray}s
}
\item \code{\link[=juliaClass]{juliaClass()}} extracts the type of a \code{Julia} object as an R \code{class}. This is used for method dispatch in \code{\link[=juliaTranslate]{juliaTranslate()}}.
\item \code{\link[=juliaTranslate]{juliaTranslate()}} is a \code{\link[JuliaCall:julia_eval]{JuliaCall::julia_eval()}} equivalent.
\itemize{
\item The default method wraps  \code{\link[JuliaConnectoR:juliaEval]{JuliaConnectoR::juliaEval()}};
\item For some object types, this may return an \code{JuliaProxy} object;
\item The \code{data.frame} method translates \code{JuliaProxy} \code{DataFrame}s to \code{data.frame}s via \code{\link[JuliaConnectoR:as.data.frame.JuliaProxy]{JuliaConnectoR::as.data.frame.JuliaProxy()}};
}
}
}
\author{
Edward Lavender
}
\keyword{internal}
