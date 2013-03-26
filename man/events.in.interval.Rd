\name{events.in.interval}
\alias{events.in.interval}
\title{compute whether or not events fell in a given interval}
\usage{
  events.in.interval(exp.start, exp.end, events)
}
\arguments{
  \item{exp.start}{the start of the exposure window}

  \item{exp.end}{the end of the exposure window}

  \item{events}{the time of the events}
}
\value{
  a vector of the same length of events; the entries are 1
  for each event that occurred in the given time window and
  0 otherwise
}
\description{
  given the boundaries of a time period and a vector of
  event times, this fn returns the count of events that
  happened within the time period. so, given a time
  period\cr [exp.start, exp.end)\cr and events\cr (event1,
  event2, ...)\cr this function returns a vector with 1's
  corresponding to events that took place in the time
  window and 0 otherwise TODO: \itemize{ \item{ add unit
  tests } \item{ explain the time intervals and how they
  are used (ie, improve documentation)} } Note that if
  there is any missingness in the exp.start or exp.end
  variables, then this assumes no exposure.
}

