\name{ObsSensitivity-package}
\alias{ObsSensitivity-package}
\docType{package}
\title{
ObsSensitivity
}
\description{
ObsSensitivity determines the power of the sensitivity analysis for an observational study that is to be analysed using the Wilcoxon signed rank statistic. It determines the sample size for a specified power; or the power for a specified sample size. These both depend on the value of Gamma-the odds of the experimental group receiving the treatment.
}
\details{
Currently there are four main functions. Obspower determines the power. Obssamplesize determines the sample size. Obssensitivity determines the sensitivity of the observational study. Obspowergraph gives a graph of the power versus the sample size for various values of Gamma.
}
\author{
Neil Diamond and Ewa Sztendur
}
\references{
Rosenbaum, P.R. 2010. "Design of Observational Studies", Springer.
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
~~ the R documentation directory ~~
\keyword{ package }
\keyword{ ~Observational Studies }
\keyword{ ~Power Analysis }
\examples{
obspower(28)
obssamplesize(1,0.8)
obssensitivity(0.5)
obspowergraph(seq(1,5,0.5),2:500,0.5)
}
