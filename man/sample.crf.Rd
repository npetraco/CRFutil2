% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation_util.R
\name{sample.crf}
\alias{sample.crf}
\title{Simulate a sample}
\usage{
sample.crf(
  crf,
  size,
  crf.sample.method,
  dress.sampleQ = T,
  data.frameQ = T,
  ...
)
}
\arguments{
\item{crf}{a crf object}

\item{size}{number of samples}

\item{crf.sample.method}{sampling method in CRF: sample.exact, .junction, .conditional, .cutset}

\item{dress.sampleQ}{use the default state names from the CRF sampler or dress up with the state names stipulated in the crf object?}

\item{data.frameQ}{whether or not to return sample as a data frame}

\item{...}{other arguments needed for CRF sampling methods}
}
\value{
A sample of configurations.
}
\description{
Wrapper for CRF simulation functions.
}
\details{
The function is a wrapper for the CRF simulation functions which knits them all
into one function and allows the user to change the default state names (1,2,...etc) into
whatever their state names are actually intended to be.
}
\examples{
XXXX

}
