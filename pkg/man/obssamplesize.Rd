\name{obssamplesize}
\alias{obssamplesize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Sample Size for the sensitivity analysis of an observational study
}
\description{
Determines the sample size required to conduct a sensitivity analysis of a matched pairs observational study with specified power.
}
\usage{
obssamplesize(gamma.val = 1, power = 0.8, tauval = 0.5, omegaval = 1, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gamma.val}{
The odds of receiving the treatment rather than the control due to the bias from an unmeasured covariate.
}
  \item{power}{
The desired power of the sensitivity analysis.
}
  \item{tauval}{
The expected difference in the means due to the treatment.
}
  \item{omegaval}{
The standard deviation of the differences.
}
  \item{alpha}{
The type I error.
}
}
\value{Sample size required.}
\references{
Rosenbaum, P. 2010. "Design of Observational Studies" Springer.
}
\author{
Neil Diamond and Ewa Sztendur
}


\examples{obssamplesize(1,0.8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~Observational Studies }
\keyword{ ~Power Analysis }
