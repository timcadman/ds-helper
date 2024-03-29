% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean-by-group.R
\name{dh.meanByGroup}
\alias{dh.meanByGroup}
\title{Describes a numeric variable by strata of another numeric grouping variable.}
\usage{
dh.meanByGroup(
  df = NULL,
  outcome = NULL,
  group_var = NULL,
  intervals = NULL,
  conns = NULL,
  checks = FALSE
)
}
\arguments{
\item{df}{Character specifying a server-side data frame.}

\item{outcome}{String specifying outcome variable within `df`.}

\item{group_var}{String specifying grouping variable within 'df'.}

\item{intervals}{Optionally, numeric vector defining how to stratify
`group_var`. Values should specify alternately lower and upper values for
each strata. If NULL, `outcome` is summarised by every unique value of
`group_var`.}

\item{conns}{DataSHIELD connections object.}

\item{checks}{Logical; if TRUE checks are performed prior to running the 
function. Default is TRUE.}
}
\value{
Tibble containing mean values for each strata of `group_var`.
}
\description{
This has similar functionality to `tapply` or the dplyr chain `group_by` %>%
`summarise`. It offers additional flexilibity over `ds.tapply` in that it
allows you to specify upper and lower values for each strata. By contrast,
ds.tapply will produce a summary based on every unique value of the grouping
variable, which may not always be what is required.
}
\seealso{
Other descriptive functions: 
\code{\link{dh.anyData}()},
\code{\link{dh.classDiscrepancy}()},
\code{\link{dh.createTableOne}()},
\code{\link{dh.defineCases}()},
\code{\link{dh.getStats}()},
\code{\link{dh.lmTab}()}
}
\concept{descriptive functions}
