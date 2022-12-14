% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transposed.R
\name{transposed}
\alias{transposed}
\title{Transponer data.frame}
\usage{
transposed(x, originalType = NULL, fieldsFactor = NULL)
}
\arguments{
\item{x}{data.frame object from R}

\item{originalType}{a numeric vector with the fields you want to keep the original types. The default value is NULL.}

\item{fieldsFactor}{a numeric vector with the fields you want to assign with Factor class. Default is \code{NULL}.}
}
\description{
Esta es una función que permite transponer de manera eficiente una data.fame con más de 2 columnas.
}
\details{
Esta funcion es ideal para obtener el metadata en los encabezados de los archivos que contienen bases de datos meteorológicas generados por el laboratorio.
La funcion recibe un data.frame horizontal \code{x} y devuelve un data.frame vertical con los campos formateados según su tipo.
}
\examples{

data("metaData")
View(metaData)

transposed(metaData,1)

}
