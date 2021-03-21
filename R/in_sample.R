#' Daily realized variance of a Portfolio Function
#'
#' Return the Daily realized variance of a Portfolio
#'
#' @param data A xts object containing the intraday prices
#' of the portfolio's assets.
#'
#' @param weights A numeric vector containing the weights of each
#' asset in the portfolio. Default = NULL. If NULL, an equally weighted
#' portfolio is assumed.
#'
#' @return
#' An xts object. The output has the following columns:
#'
#' * RC : The daily realized semicovariance of the portfolio
#' * P : The daily upside realized semicovariance of the portfolio
#' * N : The daily downside realized semicovariance of the portfolio
#' * M : The daily mixed realized semicovariance of the portfolio
#'
#' @export
#' @examples
#' # Generate a xts with the realized measures
#' realized <- drvp(data = xts_data)

drvp <- function(data, weights = NULL) {


  rSC <- highfrequency::rSemiCov(data, makeReturns = TRUE)

  dim <- matrix(unlist(lapply(rSC[[1]], function (x) dim(x)[1])))[1,1, drop = TRUE]

  if (is.null(weights)){

    weights <- as.vector(rep(1/dim, dim)) #equally weighted portfolio

  }


  P <- sapply(rSC, function(x) weights%*%x$positive%*%weights)
  N <- sapply(rSC, function(x) weights%*%x$negative%*%weights)
  M <- sapply(rSC, function(x) weights%*%x$mixed%*%weights)
  RC <- sapply(rSC, function(x) weights%*%x$rCov%*%weights)
  # P <- lapply(rSC, function(x){
  #
  #   weights%*%x$positive%*%weights
  #
  # })

  covariances <- xts::xts(cbind(RC, P, N, M), as.Date(names(rSC)))

  covariances
}

#' Daily realized semivariance of a Portfolio Function
#'
#' Return the Daily realized semivariance of a Portfolio
#'
#' @param data A xts object containing the intraday prices
#' of the portfolio's assets.
#'
#' @param weights A numeric vector containing the weights of each
#' asset in the portfolio. Default = NULL. If NULL, an equally weighted
#' portfolio is assumed.
#'
#' @return
#' An xts object. The output has the following columns:
#'
#' * PSV : The daily realized semicovariance of the portfolio
#' * NSV : The daily upside realized semicovariance of the portfolio
#'
#' @export
#' @examples
#' # Generate a xts with the realized measures
#' realized <- drsvp(data = xts_data)

drsvp <- function(data, weights = NULL) {


  rSV <- highfrequency::rSV(data, makeReturns = TRUE)

  dim <- matrix(unlist(lapply(rSV[[1]], function (x) length(x)[1])))[1,1, drop = TRUE]

  if (is.null(weights)){

    weights <- as.numeric(rep(1/dim, dim)) #equally weighted portfolio

  }


  PSV <- sapply(rSV, function(x) t(weights^2)%*%x$rSVupside)
  NSV <- sapply(rSV, function(x) t(weights^2)%*%x$rSVdownside)

  # P <- lapply(rSC, function(x){
  #
  #   weights%*%x$positive%*%weights
  #
  # })

  semivariances <- xts::xts(cbind(PSV, NSV), as.Date(names(rSV)))

  semivariances
}

#' HAR_frame Function
#'
#' Return the frame in order to provide data for regression analysis
#'
#' @param ts A xts object containing the realized measures
#'
#' @param periods A numeric vector containing the periods. Default 'c(1,5,22)'
#'
#' @return
#' An xts object containing the response variable e covariates of a HAR.
#' Each covariate X_i has three columns:
#'
#' * X_i_d: The lagged daily (t-1)
#' * X_i_w: The lagged weekly (t-2:t-5)
#' * X_i_m: The lagged monthly  (t-6:t-22)
#'
#' @export
#' @examples
#' # Realized measures
#' realized1 <- drvp(data = xts_data) # realized variances
#' realized2 <- drsvp(data = xts_data) # realized semivariances
#' realized <- xts::merge.xts(realized1, realized2, join = 'inner')
#'
#' # Generate a frame to run HAR
#' frame <- HAR_frame(ts = realized)

HAR_frame <- function(ts = NULL, periods = c(1, 5, 22)){

  if (!xts::is.xts(ts)) stop("ts must be a xts object")

  cols <- ncol(ts)

  names <- names(ts)

  m1 <- ts[ , 1, drop = FALSE]

  for (i in 1:cols) {

    len <- length(ts[,1])
    day_measure <- xts::xts(seq(from = periods[3]+1, to = len), order.by = zoo::index(ts)[((periods[3]+1):len)])
    week_measure <- xts::xts(seq(from = periods[3]+1, to = len), order.by = zoo::index(ts)[((periods[3]+1):len)])
    month_measure <- xts::xts(seq(from = periods[3]+1, to = len), order.by = zoo::index(ts)[((periods[3]+1):len)])

    k = 1

      for (j in ((periods[3]+1):len)) {
        day_measure[k] <- ts[(j - periods[1]), i, drop = FALSE]
        week_measure[k] <- sum(ts[(j - periods[2]):(j - periods[1] - 1), i, drop = FALSE])/(periods[2]-periods[1])
        month_measure[k] <- sum(ts[(j - periods[3]):(j - periods[2] - 1), i, drop = FALSE])/(periods[3]-periods[2])
        k = k + 1
      }

    m2 <- xts::merge.xts(day_measure, week_measure, month_measure)

    m1 <- xts::merge.xts(m1,m2, join = 'inner')



  }

  sup_nam_ <-  NULL

  for (v in 1:length(names)) {
    sup_nam <- paste(names[v], c('d','w','m'), sep = "_")
    sup_nam_ <- c(sup_nam_,sup_nam)
  }

  names(m1) <- c(names[1], sup_nam_)

  m1

}

# Models

# model1 <- lm(RC ~ RC_d + RC_w + RC_m, data = frame)
# extractAIC(model1)
# model2 <- lm(RC ~ ., data = frame[,-(2:4)])
# extractAIC(model2)
