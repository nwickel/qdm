\name{predict.qdm}
\alias{predict.qdm}
\title{Predictions of Quadrilateral Dissimilarity Model}
\description{
  Predicts regular minimality compliant discrimination probabilities for a
  model fitted with \code{\link{qdm}}.
}
\usage{
\method{predict}{qdm}(object, x = object$psi$x, y = object$psi$y,
        respfun = object$respfun, bias = object$bias, ...)
}
\arguments{
  \item{object}{object of class "qdm". See \code{\link{qdm}}.}
  \item{x}{stimulus intensities for the first observation area. Defaults to
    the values used for fitting the model.}
  \item{y}{stimulus intensities for the second observation area. Defaults to
    the values used for fitting the model.}
  \item{respfun}{function used to describe relationship
    between discrimination probabilities and similarity measure. Defaults to
    the function used for fitting the model. See \code{\link{qdm}} for
    Details.}
  \item{bias}{perceptual bias of subject. Defaults to the bias used for
    fitting the model.}
  \item{...}{additional arguments.}
}
\seealso{\code{\link{qdm}}}
