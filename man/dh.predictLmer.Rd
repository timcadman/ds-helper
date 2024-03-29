% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict-lmer.R
\name{dh.predictLmer}
\alias{dh.predictLmer}
\title{Gets predicted values based on a new dataframe for lmer models}
\usage{
dh.predictLmer(model = NULL, new_data = NULL, coh_names = NULL)
}
\arguments{
\item{model}{Model object returned by ds.lmerSLMA.}

\item{new_data}{Tibble or data frame containing values for variables in
`model` at which to predict values of the outcome. The column names in
`new_data` must be identical to those in `model`, and all variables included
in `model` must be present in `new_data`.}

\item{coh_names}{Please specify 'conns' instead.
Vector of cohort names. These must be in the order that
cohorts were specified in `model`.}
}
\value{
Tibble of predicted outcome values based on values provided in
`new_data`.
}
\description{
Currently there is no `predict` method for lmer models within DataSHIELD.
This function replicates this, by calculating predicted values for fixed
effects based on the model coefficients. Standard errors are returned for
individual cohorts but yet for pooled models.
}
\seealso{
Other trajectory functions: 
\code{\link{dh.lmeMultPoly}()},
\code{\link{dh.makeAgePolys}()},
\code{\link{dh.makeLmerForm}()},
\code{\link{dh.trimPredData}()},
\code{\link{dh.zByGroup}()}
}
\concept{trajectory functions}
