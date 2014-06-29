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
  \item{oa1}{vector of stimuli names in the first observation area}
  \item{oa2}{vector of stimuli names in the second observation area}
}
\examples{
data(lightness)
psi1 <- psi("vp01", lightness)
}
%\keyword{file}

