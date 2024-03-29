% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-age-polys.R
\name{dh.makeAgePolys}
\alias{dh.makeAgePolys}
\title{Produces multiple transformations of the age term for fractional polynomial
analyses}
\usage{
dh.makeAgePolys(
  df = NULL,
  age_var = NULL,
  poly_form = c("^-2", "^-1", "^-0.5", "log", "^0.5", "^2", "^3"),
  poly_names = c("_m_2", "_m_1", "_m_0_5", "log", "_0_5", "_2", "_3"),
  conns = NULL,
  checks = TRUE,
  agevars = NULL
)
}
\arguments{
\item{df}{Character specifying a server-side data frame.}

\item{age_var}{Character specifying age variable within `df` to transform.}

\item{poly_form}{Character vector of powers by which to transform `age_var`.}

\item{poly_names}{Character vector of names for the created variables. Must
the same length and order as `poly_form`.}

\item{conns}{DataSHIELD connections object.}

\item{checks}{Logical; if TRUE checks are performed prior to running the 
function. Default is TRUE.}

\item{agevars}{Retired argument name. Please use `age_var' instead.}
}
\value{
Transformations of age are added as columns to server-side object
specified in `df`.
}
\description{
When we do trajectory analyses using fractional polynomials we often want
to try models with different combinations of age polynomials. This function
creates multiple transformations of an age variable to different powers.
}
\seealso{
Other trajectory functions: 
\code{\link{dh.lmeMultPoly}()},
\code{\link{dh.makeLmerForm}()},
\code{\link{dh.predictLmer}()},
\code{\link{dh.trimPredData}()},
\code{\link{dh.zByGroup}()}

Other data manipulation functions: 
\code{\link{dh.dropCols}()},
\code{\link{dh.makeIQR}()},
\code{\link{dh.makeStrata}()},
\code{\link{dh.quartileSplit}()},
\code{\link{dh.renameVars}()},
\code{\link{dh.tidyEnv}()},
\code{\link{dh.zByGroup}()}
}
\concept{data manipulation functions}
\concept{trajectory functions}
