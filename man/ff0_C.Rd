% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{ff0_C}
\alias{ff0_C}
\title{Feature function in Rcpp}
\usage{
ff0_C(st, ss_dim = 2L, w_vec = NULL, st_vec = NULL)
}
\arguments{
\item{st}{State label}

\item{ss_dim}{State space dimension. Default is 2 (Ising)}

\item{w_vec}{Optional weight vector}

\item{st_vec}{Optional vector for state names}
}
\value{
The feature function, which is a arma vector.
}
\description{
Built-in feature function for convenience and testing. Works pretty much like ff0 on the R side.
}
\details{
The function is a handy feature function for testing. It can handle both Ising and Potts state spaces. Surprisingly though this is twice as slow as ff0....
}
\examples{
ff0_C(st = 3, ss_dim = 4, st_vec = c(0,1,2,3))
ff0_C(st = "a", st_vec = c("a","b"))
ff0_C(st = "ID", ss.dim = 3, st.vec = c("ID","exclude","inconclusive"))
ff0_C(1)
ff0_C(1,3)
ff0_C(st = "b")           # Throws error
ff0_C(st = 5)             # Throws error
ff0_C(st = 5, ss_dim = 4) # Throws error

}
