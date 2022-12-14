% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/area_of_applicability.R
\name{print.ww_area_of_applicability}
\alias{print.ww_area_of_applicability}
\title{Print number of predictors and area-of-applicability threshold}
\usage{
\method{print}{ww_area_of_applicability}(x, digits = getOption("digits"), ...)
}
\arguments{
\item{x}{A \code{ww_area_of_applicability} object.}

\item{digits}{The number of digits to print, used when rounding the AOA threshold.}

\item{...}{These dots are for future extensions and must be empty.}
}
\description{
Print number of predictors and area-of-applicability threshold
}
\examples{
\dontshow{if (rlang::is_installed("vip")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(vip)
trn <- gen_friedman(500, seed = 101)  # ?vip::gen_friedman
pp <- ppr(y ~ ., data = trn, nterms = 11)
importance <- vi_permute(
  pp,
  target = "y",
  metric = "rsquared",
  pred_wrapper = predict
)


ww_area_of_applicability(trn[2:11], importance = importance)
\dontshow{\}) # examplesIf}
}
\keyword{internal}
