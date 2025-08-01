% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crf_util.R
\name{insert.params.and.pots}
\alias{insert.params.and.pots}
\title{Input a parameter vector into parameter and potential matrices and vectors of crf object}
\usage{
insert.params.and.pots(crf, params.samp, nlpQ = T)
}
\arguments{
\item{crf}{a crf object}

\item{nlpQ}{include negative log potentials (energies) for good measure?}

\item{param.samp}{a sample of parameter space or parameter vector}
}
\value{
Nothing.
}
\description{
Input a parameter vector into parameter and potential matrices and vectors of crf object
}
\details{
Input a parameter vector into parameter and potential matrices of crf object.
By default, negative log potentials (nlp, energies) unlabeled vectors and matrices are
also tacked on. The user can sut this off with nlpQ=F. To include properly labeled
potentials and energies for use with gRbase functions cf. make.gRbase.potentials.
}
\examples{
XXXX

}
