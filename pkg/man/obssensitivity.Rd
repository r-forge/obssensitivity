\name{obssensitivity}
\alias{obssensitivity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the sensitivity of an observational study
}
\description{
Determines the design sensitivity of a matched pairs observational study.
}
\usage{
obssensitivity(tauval = 0.5, omegaval = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tauval}{
The expected difference in the means due to the treatment.
}
  \item{omegaval}{
The standard deviation of the differences.
}

}
\value{
The design sensitivity
}
\references{
Rosenbaum, P.R. 2010. "Design of Observational Studies", Springer.
}
\author{
Neil Diamond and Ewa Sztendur
}

\examples{obssensitivity(0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }
\keyword{ ~Observational Studies }
\keyword{ ~Power Analysis }