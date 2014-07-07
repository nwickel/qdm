% File src/library/base/man/qdm.Rd
\name{qdm}
\alias{qdm}
\title{Fit Quadrilateral Dissimilarity Model}
\description{
  Fits a Quadrilateral Dissimilarity Model to same-different data.
}
\usage{
qdm(psi, start, respfun = c("logistic", "guessing", "dlogistic",
    "dlogisticp", "shepardA", "shepardAneg", "shepardB", "shepardBneg",
    "shepardE", "shepardEneg", "shepardF", "shepardFneg"), estimfun =
    c("minchi2", "ols", "wls"), optimizer = c("optim", "nlm"), optimargs =
    list())
}
\arguments{
  \item{psi}{a data object created with \code{\link{psi}}.}
  \item{start}{starting values for parameter estimation.}
  \item{respfun}{function that shoul be used to describe relationship
  between discrimination probabilities and similarity measure, see Details.}
  \item{estimfun}{method to estimate parameters, see Details.}
  \item{optimizer}{which optimizer should be used: \code{\link{nlm}} or
  \code{\link{optim}}.}
  \item{optimargs}{takes addtional arguments to control \code{\link{nlm}}
  or \code{\link{optim}}.}
}
\details{
  \preformatted{respfun}{Different functions can be used to describe the
  relationship between discriminatin probabilities and dissimilarity
  measure. It can be chosen between the logistic function \code{logistic},
  the logistic function with guessing parameter \code{guessing}, the
  density function of the logistic function \code{dlogistic}, and four
  functions suggested by Shepard (1987) \code{shepardA, shepardB, shepardE,
  shepardF}. Default is the logistic function.}
  \preformatted{estimfun}{Parameters can be estimated by using different
  minimizing function: ordinary least squares, weighted least squares, and
  minimization of Pearson's \eqn{X^2}. Default is the minimization of
  \eqn{X^2}.}
}
\examples{
## load data
data(FMrate)
psi1 <- psi("vp01", FMrate)

## estimate model
p.s <- c(.2, .5, .1, .5, .3, .1, .1, .1)
q1 <- qdm(psi1, start=p.s)
print(q1)}
%\keyword{file}

