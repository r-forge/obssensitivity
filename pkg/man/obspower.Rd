\name{obspower}
\alias{obspower}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wilcoxon Matched Pairs Sensitivity Analysis Power
}
\description{
Function to calculate power of a sensitivity analysis for an observational study where a Wilcoxon matched pairs analysis is undertaken.
}
\usage{
obspower(npairs, tauval = 0.5, omegaval = 1, alpha = 0.05, gamma.val = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{npairs}{
The number of matched pairs.
}
  \item{tauval}{
The difference in the means of the experimental and control groups that is to be detected.
}
  \item{omegaval}{
The within group standard deviation.
}
  \item{alpha}{
Type I error rate with default 0.05.
}
  \item{gamma.val}{
The odds of receiving the treatment rather than the control due to bias from an unmeasured covariate.
}
}
\details{
A gamma.val=1 corresponds to a randomised matched pairs experiment.
}
\value{
The power of the sensitivity analysis.
}
\references{
Rosenbaum, P. (2010). Design of Observational Studies. Springer.
}
\author{
Neil Diamond and Ewa Sztendur
}

\examples{
obspower(28)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sensitivity Analysis }
\keyword{ Power }
\keyword{ Design of Observational Studies}
\keyword{ Matched Pairs}% __ONLY ONE__ keyword per line
