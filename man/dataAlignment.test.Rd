% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataAlignment.R
\name{dataAlignment.test}
\alias{dataAlignment.test}
\title{Alignment of the data}
\usage{
dataAlignment.test(
  X,
  Y,
  alignment = "unadjusted",
  constant = NULL,
  absolute = FALSE
)
}
\arguments{
\item{X}{vector. Monitoring sample.}

\item{Y}{vector. Reference sample.}

\item{alignment}{character string. Aligment of the data \code{X} and \code{Y}. Select from
\itemize{
  \item "unadjusted": nothing is sustracte from \code{X} and \code{Y} (default).
  \item "overallmean": overall mean is sustracted from \code{X} and \code{Y}.
  \item "overallmedian": overall median is sustracted from \code{X} and \code{Y}.
  \item "samplemean": mean from corresponding group (\code{X} and \code{Y}) is sustracted from its corresponing vector.
  \item "samplemedian": median from corresponding group (\code{X} and \code{Y}) is sustracted from its corresponing vector.
  \item "referencemean": mean from \code{Y} is sustracted from \code{X} and \code{Y}.
  \item "referencemedian": median from \code{Y} is sustracted from \code{X} and \code{Y}.
  \item "constantvalue": a constant value is sustracted from \code{X} and \code{Y}.
}}

\item{constant}{scalar. Only used when the \code{alignment} is selected "constantvalue". Default \code{NULL}.}

\item{absolute}{logical. If \code{TRUE}, the absolute aligned values are obtained. (Default \code{FALSE})}
}
\description{
Align the monitoring sample \code{X} and the reference sample \code{Y}.
}
\examples{
X = c(30, 45, 50)
Y = c(20, 22, 25, 30, 70)
dataAlignment.test(X,Y)

}
