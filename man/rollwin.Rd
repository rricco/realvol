% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/out_of_sample.R
\name{rollwin}
\alias{rollwin}
\title{Rolling window Analysis}
\usage{
rollwin(data = NULL, length_training_data = NULL)
}
\arguments{
\item{data}{A xts object containing the frame
. Default = NULL.}

\item{length_training_data}{A numeric representing
the length of the training data. (0 < length_training_data < 1).}
}
\value{
A list containing the one-step-ahead forecast of each model.
}
\description{
Return the forecasts of each Model considered in Bollerslev, Li, Patton, et al. (2020).
}
\examples{
# Realized measures
realized1 <- drvp(data = xts_data) # realized variances
realized2 <- drsvp(data = xts_data) # realized semivariances
realized <- xts::merge.xts(realized1, realized2, join = 'inner')

# Generate a frame to run HAR
frame <- HAR_frame(ts = realized)

# Rolling window Analysis
forecasts <- rollwin(data = frame, length_training_data = 0.9)

}
\references{
BOLLERSLEV, Tim; LI, Jia; PATTON, Andrew J, et al. Realized semicovariances.
Econometrica, Wiley Online Library, v. 88, n. 4, p. 1515–1551, 2020
}
