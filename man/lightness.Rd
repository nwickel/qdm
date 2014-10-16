\name{lightness}
\docType{data}
\alias{lightness}
\title{Same-different judgments on gray patches}
\description{
  This data set consists of same-different judgments of four subjects on 9
  gray patches (luminance of 37.35, 38.69, 40.05, 41.43, 42.88, 44.34,
  45.89, 47.45, 49.07 cd/m^2) presented side by side under a constant
  illumination (133.27 cd/m^2).
}
\usage{data(lightness)}
\format{data frame of 7 variables and 35,760 observations.}
\details{The 7 variables are named 'id', 'ses', 'resp', 'RT', 'key', 's1',
and 's2'.
\preformatted{id}{subject id.}
\preformatted{ses}{each subject performed 15 sessions. Gives number of
session.}
\preformatted{resp}{gives the response of a subject in each trial. Can
either be 's' for same or 'd' for different.}
\preformatted{RT}{reaction time from stimulus onset until reaction of
subject.}
\preformatted{key}{which mouse button was pressed. Assigment of same and
different to the mouse buttons varied between subjects.}
\preformatted{s1}{stimulus which was presented in the first observation area; in
this case: on the left side of the monitor.}
\preformatted{s2}{stimulus which was presented in the second observation
area (right side).}
}
\source{
  Umbach, N (2013). \emph{Dimensionality of the Perceptual Space of
  Achromatic Surface Colors.} Dissertation, University of Tuebingen.
}
\keyword{datasets}

