% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{preparar_json_alumno}
\alias{preparar_json_alumno}
\title{Manipula json de eventos para un alumno}
\usage{
preparar_json_alumno(json_data)
}
\arguments{
\item{json_data}{A string}
}
\value{
una lista con los objetos necesarios para el cuadro de mandos shiny de alumno
}
\description{
Manipula json de eventos para un alumno
}
\examples{
preparar_json_alumno({{"a":3,"nombre":"Mathieu"} })
}
