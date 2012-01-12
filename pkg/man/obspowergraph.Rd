\name{obspowergraph}
\alias{obspowergraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power graph for an Observational Study
}
\description{
Draws the power graph for various values of Gamma.
}
\usage{
powergraph(gammavalues, numb.pairs, tauval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gammavalues}{
Values of Gamma for which the power graph is to be drawn.
}
  \item{numb.pairs}{
Sequence of number of matched pairs.
}
  \item{tauval}{
The effect of the treatment in standard deviation units.
}
}

\value{Null. The side effect of the function is to draw the graph.
}
\references{
Rosenbaum, P.R. 2010 "Design of Observational Studies"
}
\author{
Neil Diamond and Ewa Sztendur
}
\examples{
obspowergraph(seq(1,5,0.5),2:500,0.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }
\keyword{ ~Observational Studies }
\keyword{ ~Power Analysis }