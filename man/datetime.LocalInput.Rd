% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{datetime.LocalInput}
\alias{datetime.LocalInput}
\title{输入本地的时间，}
\usage{
datetime.LocalInput(x, timeZone = Sys.timezone())
}
\arguments{
\item{x}{输入长日期的日期与时分秒部分}

\item{timeZone}{时区}
}
\value{
返回当地存储的日期数据，非utc
}
\description{
输入本地的时间，
}
\examples{
datetime.LocalInput('2018-12-31 12:21:22');
}
