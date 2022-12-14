% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_geary.R
\name{ww_global_geary_c}
\alias{ww_global_geary_c}
\alias{ww_global_geary_c_vec}
\alias{ww_global_geary_pvalue}
\alias{ww_global_geary_pvalue_vec}
\alias{ww_global_geary}
\title{Global Geary's C statistic}
\usage{
ww_global_geary_c(data, ...)

ww_global_geary_c_vec(
  truth,
  estimate,
  wt = NULL,
  alternative = "greater",
  randomization = TRUE,
  na_rm = TRUE,
  ...
)

ww_global_geary_pvalue(data, ...)

ww_global_geary_pvalue_vec(
  truth,
  estimate,
  wt = NULL,
  alternative = "greater",
  randomization = TRUE,
  na_rm = TRUE,
  ...
)

ww_global_geary(
  data,
  truth,
  estimate,
  wt = NULL,
  alternative = "greater",
  randomization = TRUE,
  na_rm = TRUE,
  ...
)
}
\arguments{
\item{data}{A \code{data.frame} containing the columns specified by the \code{truth}
and \code{estimate} arguments.}

\item{...}{Additional arguments passed to \code{\link[spdep:geary.test]{spdep::geary.test()}}.}

\item{truth}{The column identifier for the true results
(that is \code{numeric}). This should be an unquoted column name although
this argument is passed by expression and supports
\link[rlang:topic-inject]{quasiquotation} (you can unquote column
names). For \verb{_vec()} functions, a \code{numeric} vector.}

\item{estimate}{The column identifier for the predicted
results (that is also \code{numeric}). As with \code{truth} this can be
specified different ways but the primary method is to use an
unquoted variable name. For \verb{_vec()} functions, a \code{numeric} vector.}

\item{wt}{A "listw" object, for instance as created with \code{\link[=ww_build_weights]{ww_build_weights()}}.}

\item{alternative}{a character string specifying the alternative hypothesis, must be one of "greater" (default), "less" or "two.sided".}

\item{randomization}{variance of I calculated under the assumption of randomisation, if FALSE normality}

\item{na_rm}{A \code{logical} value indicating whether \code{NA}
values should be stripped before the computation proceeds.}
}
\value{
A tibble with columns .metric, .estimator, and .estimate and \code{nrow(data)} rows of values.
For grouped data frames, the number of rows returned will be the same as the number of groups.
For \verb{_vec()} functions, a single value (or NA).
}
\description{
Calculate the global Geary's C statistic for model residuals.
\code{ww_global_geary_c()} returns the statistic itself, while
\code{ww_global_geary_pvalue()} returns the associated p value.
\code{ww_global_geary()} returns both.
}
\examples{
\dontshow{if (rlang::is_installed("sfdep")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
data(guerry, package = "sfdep")

guerry_modeled <- guerry
guerry_lm <- lm(crime_pers ~ literacy, guerry_modeled)
guerry_modeled$predictions <- predict(guerry_lm, guerry_modeled)

\dontrun{
ww_global_geary(guerry_modeled, crime_pers, predictions)
}
\dontshow{\}) # examplesIf}
}
