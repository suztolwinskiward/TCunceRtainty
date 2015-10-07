\name{TCwindUQR-package}
\alias{TCwindUQR-package}
\alias{TCwindUQR}
\docType{package}
\title{Quantify uncertainty in observed tropical cyclone winds}
\description{
This micro-package is really just 3 utilities that work together to take a set of observed TC windspeed observations (say, from HURDAT2 or ibtracs), and returns a vector of associated standard deviations for each observation.  The values from from the work of Ryan Torn and Chris Snyder in their 2012 Weather and Forecasting paper,"Uncertaintyof Tropical Cyclone Best-Track Information" (doi:10.1175/WAF-D-11-00085.1).
}
\details{
\tabular{ll}{
Package: \tab TCwindUQR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-10-07\cr
License: \tab MIT \cr
}
3 functions in the package are TornSnyder2012_uncert, ss_category, and v_convert. Respectively, these return the associated uncertainty with input winds, determine the Saffir-Simpson category based on input (v_max) winds, and convert units between knots, mph, and m/s.  They are fully documented.
}
\author{
Suz Tolwinski-Ward
Maintainer: Suz Tolwinski-Ward <STolwinski-Ward@air-worldwide.com>
}
\references{
Torn, R. D. and C. Snyder, 2012: Uncertainty of tropical cyclone best track information. Wea. Forecasting, 27, 715-729, doi:10.1175/WAF-D-11-00085.1
}
\keyword{ Tropical cyclones, uncertainty }
