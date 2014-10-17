\name{FMrate}
\docType{data}
\alias{FMrate}
\title{Discrimination Judgments of Frequency-Modulated Tones}
\description{
  Three subjects were repeatedly presented with all possible pairs of nine
  frequency-modulated (FM) tones. The tones had one out of nine different
  modulation rates, a random modulation phase, a duration of 500 ms, a sound
  pressure level of 60 dB, and a center frequency of 1000 Hz. 

  On each trial, subjects judged whether the two FM tones sounded same or
  different.
}
\usage{data(FMrate)}
\format{
  A data frame of eight variables and 26,820 observations:
  \describe{
    \item{\code{resp}}{the response of a subject in each trial, either
      \code{s} for same or \code{d} for different.}
    \item{\code{RT}}{reaction time from stimulus onset until reaction of
      subject.}
    \item{\code{id}}{subject id.}
    \item{\code{ses}}{number of experimental session.}
    \item{\code{s1}}{stimulus (FM rate in Hz) which was presented in the first
      observation area; in this case: first.}
    \item{\code{s2}}{stimulus (FM rate in Hz) which was presented in the
      second observation area.}
    \item{\code{phi1}}{random modulation phase shift for stimulus presented in
      the first observation area.}
    \item{\code{phi2}}{random modulation phase shift for stimulus presented in
      the second observation area.}
  }
}
\source{
  Umbach, N., & Wickelmaier, F. (2014).
  Violations of regular minimality in discrimination judgments of
  frequency-modulated tones.
  Presented at the \emph{45th European Mathematical Psychology Group (EMPG)
  Meeting}, July 30 to August 1, Tuebingen, Germany.
}
\examples{
data(FMrate)
xtabs(~ id + ses, FMrate)
}
\keyword{datasets}

