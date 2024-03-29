% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-strata.R
\name{dh.makeStrata}
\alias{dh.makeStrata}
\title{Creates strata of a repeated measures variable within specified age or time
bands}
\usage{
dh.makeStrata(
  df = NULL,
  id_var = NULL,
  age_var = NULL,
  var_to_subset = NULL,
  bands = NULL,
  mult_action = NULL,
  mult_vals = NULL,
  keep_vars = NULL,
  new_obj = NULL,
  band_action = NULL,
  conns = NULL,
  checks = TRUE,
  df_name = NULL
)
}
\arguments{
\item{df}{Character specifying a server-side data frame.}

\item{id_var}{Character giving the name of the column within `df` which 
uniquely identifies each subject.}

\item{age_var}{Character specifying age or time variable in \code{df}.}

\item{var_to_subset}{Character specifying variable in \code{df} to stratify
according to bands.}

\item{bands}{Numeric vector of alternating lower and upper values specifying
the bands in which to derive strata of \code{var_to_subset}. This vector should
be an even number and twice the length of the number of bands required.}

\item{mult_action}{Character specifying how to handle cases where a subject
has more than one measurement within a specified band. Use "earliest" to
take the earliest measurement, "latest" to take the latest measurement and
"nearest" to take the measurement nearest to the value(s) specified in
\code{mult_vals}.}

\item{mult_vals}{Numeric vector specifying the value in each age band to
chose values closest to if subjects have more than one value per band.
Required only if mult_action is "nearest". The order and length of the vector
should correspond to the order and number of the bands.}

\item{keep_vars}{Optionally, a vector of variable names within df to include
within each strata created.}

\item{new_obj}{Character specifying name for created serverside object.}

\item{band_action}{Character specifying how the values provided in \code{bands}
are evaluated in creating the strata:
\itemize{
\item "g_l" = greater than the lowest band and less than the highest band
\item "ge_le" = greater or equal to the lowest band and less than or equal to the
highest band
\item "g_le" = greater than the lowest band and less than or equal to the highest
band
\item "ge_l" = greater than or equal to the lowest band and less than the highest
band
}}

\item{conns}{DataSHIELD connections object.}

\item{checks}{Logical; if TRUE checks are performed prior to running the 
function. Default is TRUE.}

\item{df_name}{Retired argument name. Please use `new_obj' instead.}
}
\value{
Servside dataframe in wide format containing the derived variables.
For each band specified at least two variables will be returned:
\itemize{
\item var_to_subset
\item age_var.
The suffix .lower_band identifies the band for that variable.
}

If argument \code{keep_vars} is not NULL, then additional variables will be
added to the data frame representing these variables within the strata
created.
}
\description{
For many analyses you may want to create strata of repeated measures data
within specified bands. For example, you may have BMI measures between ages
0-18, but want to create a variable for each subject which is their BMI
between ages 9-11. This function automates this process.
}
\details{
The steps here are equivalent to the following dplyr chain:

df \%>\%
group_by(band, id) \%>\%
arrange() \%<\%
slice(1)

One of the complexities of this operation is how to deal with cases where
subjects have multiple observations within a specified band. This is handled
by first sorting the group so that the required value is first. When the
data is reshaped to wide format all but the first value for subjects with
multiple observations within a band are dropped.

Note that for big datasets this will take a long time to run.
}
\seealso{
Other data manipulation functions: 
\code{\link{dh.dropCols}()},
\code{\link{dh.makeAgePolys}()},
\code{\link{dh.makeIQR}()},
\code{\link{dh.quartileSplit}()},
\code{\link{dh.renameVars}()},
\code{\link{dh.tidyEnv}()},
\code{\link{dh.zByGroup}()}
}
\concept{data manipulation functions}
