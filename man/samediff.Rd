\name{psi}
\alias{psi}
\alias{samediff}
\title{Create Discrimination Probabilities (Psi) from Data Frame}
\description{
  Creates matrices with discrimination probabilities, number of trials, number
  of different answers, and stimulus intensities from a data frame.
}
\usage{
psi(data, oa1 = "s1", oa2 = "s2", resp = "resp")
}
\arguments{
  \item{data}{data frame with repeated same-different judgments.}
  \item{oa1, oa2}{name of the variable that contains the stimulus intensities
    in the first and second observation area.}
  \item{resp}{name of response variable for same-different judgments;
    answers have to be \code{d} and \code{s} for "different" and "same"
    response, respectively.}
}
\details{
  The data frame must include at least a column for the response and one
  column each for observation areas one and two.  The return value of
  \code{psi} may serve as an input to \code{\link{qdm}} that fits a
  quadrilateral dissimilarity model to the judgments.
}
\value{
  An object of class \code{psi} that consists of the following components:

  \item{prob}{a matrix of discrimination probabilities per stimulus
    combination.}
  \item{ntrials}{a matrix of number of trials per stimulus combination.}
  \item{freq}{a matrix of frequencies of different answers per stimulus
    combination.}
  \item{x, y}{(if possible numeric) vectors of stimulus intensities.}
}
\seealso{
  \code{\link{qdm}}.
}
\examples{
data(FMrate)
psi1 <- psi(FMrate[FMrate$id == "subj1",])
}
