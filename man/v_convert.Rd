% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/funcs.r
\name{v_convert}
\alias{v_convert}
\title{Convert wind speed units.}
\usage{
v_convert(v, from, to)
}
\arguments{
\item{v}{The wind speed}

\item{from}{Units from which to convert. "kt", "m/s", or "mph".}

\item{to}{Units to which to convert. "kt", "m/s", or "mph".}
}
\value{
v Wind speed in converted units.
}
\description{
\code{v_convert} converts windspeeds between knots, m/s, and mph.
}

