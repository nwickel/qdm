\name{lightness}
\docType{data}
\alias{lightness}
\title{Discrimination Judgments of Gray Patches}
\description{
  Four subjects were repeatedly presented with all possible pairs of nine
  gray patches presented under a constant illumination of 133.27 cd/m^2.
  Stimuli had a size of 0.81 degrees of visual angle and were presented side
  by side with a distance of 2.32 degrees of visual angle.
  
  On each trial, subjects judged whether the two patches looked same or
  different.
}
\usage{data(lightness)}
\format{A data frame of seven variables and 35,760 observations:
  \describe{
    \item{\code{resp}}{the response of a subject in each trial, either \code{s}
    for same or \code{d} for different.}
    \item{\code{RT}}{reaction time from stimulus onset until reaction of
      subject.}
    \item{\code{id}}{subject id.}
    \item{\code{ses}}{number of experimental session.}
    \item{\code{s1}}{stimulus which was presented in the first observation area; in
      this case: on the left side of the monitor.}
    \item{\code{s2}}{stimulus which was presented in the second observation
      area.}
    \item{\code{key}}{which mouse button was pressed. Assigment of same and
      different to the mouse buttons varied between subjects.}
  }
}
\source{
  Umbach, N (2013). \emph{Dimensionality of the Perceptual Space of
  Achromatic Surface Colors.} Dissertation, University of Tuebingen.
}
\examples{
data(lightness)
xtabs(~ id + ses, lightness)
}

\keyword{datasets}

