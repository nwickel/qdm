% File src/library/base/man/qdm.Rd
\name{qdm}
\alias{qdm}
\title{Fit Quadrilateral Dissimilarity Model}
\description{
  Fits a Quadrilateral Dissimilarity Model to same-different data.
}
\usage{
qdm(psi, start, respfun = c("logistic", "guessing", "dlogistic",
    "shepardA", "shepardB", "shepardE", "shepardF"), estimfun =
    c("minchi2", "ols", "wls"), optimizer = c("optim", "nlm"), optimargs =
    list(), ...)
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
  \item{...}{further arguments passed to or from other methods.}
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
\dontrun{
## load data
data(lightness)
psi1 <- psi(lightness)

## estimate model
q1 <- qdm(psi1$psi)
print(q1)}
}
%\keyword{file}

