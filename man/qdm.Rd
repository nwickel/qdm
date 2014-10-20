\name{qdm}
\alias{qdm}
\title{Fit a Quadrilateral Dissimilarity Model}
\description{
  Fits a Quadrilateral Dissimilarity Model to same-different data.
}
\usage{
qdm(psi, start, respfun = c("logistic", "guessing", "gumbel", "gompertz",
    "weibull", "cauchy", "shepardA", "shepardAneg", "shepardB",
    "shepardBneg", "shepardD", "shepardDneg", "shepardE", "shepardEneg",
    "shepardF", "shepardFneg"), bias = 0,
    estimfun = c("minchi2", "ols", "wls"), optimizer = c("optim", "nlm"),
    optimargs = list())
}
\arguments{
  \item{psi}{data object created with \code{\link{psi}}.}
  \item{start}{starting values for parameter estimation.}
  \item{respfun}{function that describes relationship between discrimination
    probabilities and similarity measure, see Details.}
  \item{bias}{takes perceptual bias into account. Default is 0.}
  \item{estimfun}{method to estimate parameters, see Details.}
  \item{optimizer}{which optimizer should be used: \code{\link{nlm}} or
    \code{\link{optim}}.}
  \item{optimargs}{takes additional arguments passed to \code{\link{nlm}}
    or \code{\link{optim}}.}
}
\details{
  More details about the Quadrilateral Dissimilarity Model can be found in
  Dzhafarov and Colonius (2006).
  
  Via \code{respfun}, different functions can be selected to describe the
  relationship between discrimination probabilities and dissimilarity
  measure. Implemented are the logistic function (\code{logistic}),
  the logistic function with guessing parameter (\code{guessing}), several
  other functions commonly used as psychometric functions (\code{gumbel},
  \code{gompertz}, \code{weibull}, \code{cauchy}), and five functions
  suggested by Shepard (1987) (\code{shepardA}, \code{shepardB},
  \code{shepardD}, \code{shepardE}, \code{shepardF}) and their negatives
  (\code{shepardAneg}, \code{shepardBneg}, \code{shepardDneg}
  \code{shepardEneg}, \code{shepardFneg}). Default is the logistic
  function. Note that for some of these functions the results critically
  depend on the choice of the starting values.

  Parameters can be estimated by using different minimizing functions
  available via the \code{estimfun} argument: ordinary least squares
  (\code{ols}), weighted least squares (\code{wls}), and minimization of
  Pearson's \eqn{X^2} (\code{minchi2}). Default is the minimization of
  \eqn{X^2}.
}
\value{
  An object of class \code{qdm} that consists of the following components:

  \item{optimout}{output of optimizer (\code{\link{nlm}} or
    \code{\link{optim}}).}
  \item{coefficients}{estimated parameters.}
  \item{psi}{\code{psi} object used to fit Quadrilateral Dissimilarity 
    Model.}
  \item{respfun}{function used to describe relationship between discrimination
    probabilities and similarity measure.}
  \item{bias}{perceptual bias used in the model.}
}
\references{
  Dzhafarov, E. N., & Colonius, H. (2006). Regular Minimality: A fundamental
  law of discrimination. In H. Colonius & E. N. Dzhafarov (Eds.),
  \emph{Measurement and representation of sensations} (pp. 1--46).
  Hillsdale, NJ: Lawrence Erlbaum Associates.

  Shepard, R. N. (1987). Towards a universal law of generalization for
  psychological science. \emph{Science, 237}, 1317--1323.
}
\seealso{
  \code{\link{psi}}, \code{\link{predict.qdm}}, \code{\link{persp.qdm}},
  \code{\link{nlm}}, \code{\link{optim}}.
}
\examples{
## prepare data
data(FMrate)
psi1 <- psi(FMrate[FMrate$id == "subj1",])

## estimate model
p.s <- c(.2, .5, .1, .5, .3, .1, .1, .1)
q1 <- qdm(psi1, start=p.s)
print(q1)

## model predictions
predict(q1)
persp(q1)
}

