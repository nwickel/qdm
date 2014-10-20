\name{persp.qdm}
\alias{persp.qdm}
\title{Perspective Plot for Predictions of Quadrilateral Dissimilarity Model}
\description{
  Draws a perspective plot of the predictions of a Quadrilateral
  Dissimilarity Model fitted with \code{\link{qdm}}.
}
\usage{
\method{persp}{qdm}(x, col = "gray", zlim = 0:1, phi = 10, theta = -25,
      xlab = "OA1", ylab = "OA2", zlab = "Predictions", ...)
}
\arguments{
  \item{x}{object of class 'qdm'. See \code{\link{qdm}}.}
  \item{col}{the color(s) of the surface facets.  Transparent colours are
        ignored. This is recycled to the (nx-1)(ny-1) facets.}
  \item{zlim}{Defaults to zlim=c(0,1) since probabilities are plotted.}
  \item{theta, phi}{angles defining the viewing direction. 'theta' gives the
        azimuthal direction and 'phi' the colatitude.}
  \item{xlab, ylab, zlab}{titles for the axes. N.B. These must be character
        strings; expressions are not accepted. Numbers will be
        coerced to character strings.}
  \item{...}{additional graphical parameters (see 'par').}
}
\seealso{\code{\link{qdm}}, \code{\link{persp}}}
