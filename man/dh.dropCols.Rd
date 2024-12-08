% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop-cols.R
\name{dh.dropCols}
\alias{dh.dropCols}
\title{Removes columns from a serverside data frame}
\usage{
dh.dropCols(
  df = NULL,
  vars = NULL,
  new_obj = NULL,
  type = NULL,
  conns = NULL,
  checks = TRUE,
  new_df_name = NULL
)
}
\arguments{
\item{df}{Character specifying a server-side data frame.}

\item{vars}{Character vector specifying columns within \code{df} to be removed or
kept.}

\item{new_obj}{Optionally, character specifying name for new server-side
data frame. Default is to return original data frame with columns removed.}

\item{type}{Character specifying how to treat \code{vars}. If "remove" these
variables are removed from the data frame, if "keep" these variables are
kept in the data frame and all others are removed.}

\item{conns}{DataSHIELD connections object.}

\item{checks}{Logical; if TRUE checks are performed prior to running the
function. Default is TRUE.}

\item{new_df_name}{Retired argument name. Please use `new_obj' instead.}
}
\value{
Server-side data frame the specified subset of columns.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function allowed you to subset a data frame by column names. It was deprecated
because you can now use \code{dsTidyverseClient::ds.select()} which is much quicker and has greater
flexibility.
}
\keyword{internal}
