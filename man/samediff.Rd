% File src/library/base/man/samediff.Rd
\name{psi}
\alias{psi}
\title{Create discrimination probabilities from data file}
\description{
  Creates a list of matrices with the discrimination probabilities, number
  of trials, number of different answers, and stimuli names from a
  data frame.
}
\usage{
psi(subj, dat)
}
\arguments{
  \item{subj}{id of certain subject}
  \item{dat}{data frame}
}
\examples{
\dontrun{
## load data
data(lightness)
psi1 <- psi(lightness)
}
}
%\keyword{file}

