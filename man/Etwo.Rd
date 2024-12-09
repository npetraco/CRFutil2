% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy_util.R
\name{Etwo}
\alias{Etwo}
\title{Two-body (edge) energy function}
\usage{
Etwo(yA, yB, wAB, ff, ...)
}
\arguments{
\item{yA}{node A state}

\item{yB}{node B state}

\item{wAB}{omega matrix}

\item{ff}{feature function}

\item{...}{optional arguments for feature function}
}
\value{
Two-body (edge) energy.
}
\description{
Two-body (edge) energy function
}
\details{
Two-body (edge) energy function
}
\examples{
# edge weights:
omega22 <- rbind(
 c( 3.5, -1.4),
 c(-1.4,  2.5)
)

omega33 <- rbind(
 c( 3.5, -1.4,   2.5),
 c(-1.4,  2.5,  -0.8),
 c( 3.8,  0.95, -7.6)
)

Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1, nss.dim=2)
Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1, nss.dim=3) # Throws error
Etwo(yA = 2, yB = 2, wAB = omega33, ff = ff1, nss.dim=3)
Etwo(yA = "c", yB = "b", wAB = omega33, ff = ff1, nss.vec=c("a", "b", "c"))

Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff0)
Etwo(yA = 2, yB = 1, wAB = omega33, ff = ff0)
Etwo(yA = 2, yB = 1, wAB = omega33, ff = ff1) # Throws error bec nss.dim=2 assumed
Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1)

}
