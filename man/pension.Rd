% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg-package.R
\docType{data}
\name{pension}
\alias{pension}
\alias{401(k)}
\alias{plans}
\alias{wealth}
\alias{data}
\title{Pension 401(k) data set}
\format{
Dataframe with the following variables (amongst others): \describe{
\item{p401}{participation in 401(k)}
\item{e401}{eligibility for 401(k)}
\item{a401}{401(k) assets}
\item{tw}{total wealth (in US $)}
\item{tfa}{financial assets (in US $)}
\item{net_tfa}{net financial assets (in US $)}
\item{nifa}{non-401k financial assets (in US $)}
\item{net_nifa}{net non-401k financial assets}
\item{net_n401}{net non-401(k) assets (in US $)}
\item{ira}{individual retirement account (IRA)}
\item{inc}{income (in US $)}
\item{age}{age}
\item{fsize}{family size}
\item{marr}{married}
\item{pira}{participation in IRA} \item{db}{defined benefit pension}
\item{hown}{home owner} \item{educ}{education (in years)} \item{male}{male}
\item{twoearn}{two earners} %\item{i1-i7}{} %\item{a1-a5}{}
\item{nohs, hs, smcol, col}{dummies for education: no high-school, high-school, some college, college}
\item{hmort}{home mortage (in US $)}
\item{hequity}{home equity (in US $)} \item{hval}{home value (in US $)}}
}
\description{
Data set on financial wealth and 401(k) plan participation
}
\details{
The sample is drawn from the 1991 Survey of Income and Program Participation
(SIPP) and consists of 9,915 observations. The observational units are
household reference persons aged 25-64 and spouse if present. Households are
included in the sample if at least one person is employed and no one is
self-employed. The data set was analysed in Chernozhukov and Hansen (2004)
and Belloni et al. (2014) where further details can be found. They examine
the effects of 401(k) plans on wealth using data from the Survey of Income
and Program Participation using 401(k) eligibility as an instrument for
401(k) participation.
}
\examples{
data(pension)
}
\references{
V. Chernohukov, C. Hansen (2004). The impact of 401(k)
participation on the wealth distribution: An instrumental quantile
regression analysis. The Review of Economic and Statistics 86 (3), 735--751.

A. Belloni, V. Chernozhukov, I. Fernandez-Val, and C. Hansen (2014). Program
evaluation with high-dimensional data. Working Paper.
}
\keyword{401(k)}
\keyword{datasets}
\keyword{pension}
