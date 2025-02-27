% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayestools.R
\name{invert_bt}
\alias{invert_bt}
\title{Perform Bayesian inversion using BayesianTools package}
\usage{
invert_bt(observed, model, prior, custom_settings = list(), loglike = NULL)
}
\arguments{
\item{observed}{Vector of observations. Ignored if \code{loglike} is not \code{NULL}.}

\item{model}{Function called by log-likelihood. Must be \verb{function(params)}
and return a vector equal to \code{length(observed)} or \code{nrow(observed)}. Ignored
if \code{loglike} is not \code{NULL}.}

\item{prior}{BayesianTools prior object.}

\item{custom_settings}{Nested settings list. See Details.}

\item{loglike}{Custom log likelihood function. If \code{NULL}, use \code{\link[=rtm_loglike]{rtm_loglike()}}
with provided \code{observed} and \code{model}.}
}
\description{
Use samplers from the BayesianTools package to fit models to data. Like
\code{invert.auto}, this will continue to run until convergence is achieved
(based on Gelman diagnostic) \emph{and} the result has enough samples (as
specified by the user; see Details).
}
\details{
\code{custom_settings} is a list of lists, containing the following:
\itemize{
\item \code{common} -- BayesianTools settings common to both the initial and subsequent samples.
\item \code{init} -- BayesianTools settings for just the first round of sampling.
This is most common for the initial number of iterations, which is the
minimum expected for convergence.
\item \code{loop} -- BayesianTools settings for iterations inside the convergence
checking \code{while} loop. This is most commonly for setting a smaller
iteration count than in \code{init}.
\item \code{other} -- Miscellaneous (non-BayesianTools) settings, including:
\itemize{
\item \code{sampler} -- String describing which sampler to use. Default is \code{DEzs}
\item \code{use_mpsrf} -- Use the multivariate PSRF to check convergence.
Default is \code{FALSE} because it may be an excessively conservative
diagnostic.
\item \code{min_samp} -- Minimum number of samples after burnin before stopping.
Default is 5000.
\item \code{max_iter} -- Maximum total number of iterations. Default is 1e6.
\item \code{lag.max} -- Maximum lag to use for autocorrelation normalization.
Default is \code{10 * log10(n)} (same as \code{stats::acf} function).
\item \code{save_progress} -- File name for saving samples between loop
iterations. If \code{NULL} (default), do not save progress samples.
\item \code{threshold} -- Threshold for Gelman PSRF convergence diagnostic. Default is 1.1.
\item \code{verbose_loglike} -- Diagnostic messages in log likelihood output. Default is TRUE.
}
}

See the BayesianTools sampler documentation for what can go in the \code{BayesianTools} settings lists.
}
