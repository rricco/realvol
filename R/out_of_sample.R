#' Rolling window Analysis
#'
#' Return the forecasts of each Model considered in Bollerslev, Li, Patton, et al. (2020).
#'
#' @param data A xts object containing the frame
#' . Default = NULL.
#'
#' @param length_training_data A numeric representing
#' the length of the training data. (0 < length_training_data < 1).
#'
#' @return
#' A list containing the one-step-ahead forecast of each model.
#'
#' @examples
#' # Realized measures
#' realized1 <- drvp(data = xts_data) # realized variances
#' realized2 <- drsvp(data = xts_data) # realized semivariances
#' realized <- xts::merge.xts(realized1, realized2, join = 'inner')
#'
#' # Generate a frame to run HAR
#' frame <- HAR_frame(ts = realized)
#'
#' # Rolling window Analysis
#' forecasts <- rollwin(data = frame, length_training_data = 0.9)
#'
#' @references
#' BOLLERSLEV, Tim; LI, Jia; PATTON, Andrew J, et al. Realized semicovariances.
#' Econometrica, Wiley Online Library, v. 88, n. 4, p. 1515–1551, 2020
#' @export

rollwin <- function(data = NULL, length_training_data = NULL){

  # Support function
  fit_predict <- function(response = NULL, covariates = NULL){

    if (is.null(response) && is.null(covariates)){

      stop("response and covariates are needed")

    }

    # Fitting using training data
    const <- xts::xts(seq(1, 1, length.out = dim(covariates)[1]), order.by = zoo::index(covariates))
    x.dt <- cbind(const,covariates)
    coeff = solve(t(x.dt)%*%x.dt)%*%t(x.dt)%*%response
    coeff = as.vector(coeff)

    # Getting the last observation

    x.vaux <- as.vector(c(1, covariates[dim(covariates)[1], ]))

    # Predicting one-step-ahead
    y.h <- coeff%*%x.vaux

    as.numeric(y.h)
  }

  # Number of windows
  w_size = round(length_training_data*dim(data)[1])
  nr <- dim(data)[1]
  n_windows <- nr - w_size

  # Getting actual values
  y.r <- data[((w_size + 1):nr), 1, drop = FALSE]

  # HAR
  y.f_HAR = xts::xts(seq(1,1, length.out = dim(y.r)[1]), order.by = zoo::index(y.r))
  for (i in 1:n_windows) {
    response = data[i:(nr-n_windows+i-1), 1, drop = FALSE]
    covariates = data[i:(nr-n_windows+i-1), c('RC_d', 'RC_w', 'RC_m'), drop = FALSE]
    fit <- fit_predict(response, covariates)
    y.f_HAR[i,1] <- fit

  }

  # SHAR
  y.f_SHAR = xts::xts(seq(1,1, length.out = dim(y.r)[1]), order.by = zoo::index(y.r))
  for (i in 1:n_windows) {
    response = data[i:(nr-n_windows+i-1), 1, drop = FALSE]
    # covariates = data[i:(nr-n_windows+i-1), c('PSV_d', 'PSV_w', 'PSV_m', 'NSV_d', 'NSV_w', 'NSV_m'), drop = FALSE]
    covariates = data[i:(nr-n_windows+i-1), c('PSV_d', 'NSV_d', 'RC_w', 'RC_m'), drop = FALSE]
    fit <- fit_predict(response, covariates)
    y.f_SHAR[i,1] <- fit

  }

  # SCHAR
  y.f_SCHAR = xts::xts(seq(1,1, length.out = dim(y.r)[1]), order.by = zoo::index(y.r))
  for (i in 1:n_windows) {
    response = data[i:(nr-n_windows+i-1), 1, drop = FALSE]
    covariates = data[i:(nr-n_windows+i-1), c('P_d', 'P_w', 'P_m', 'N_d', 'N_w', 'N_m', 'M_d', 'M_w', 'M_m'), drop = FALSE]
    fit <- fit_predict(response, covariates)
    y.f_SCHAR[i,1] <- fit

  }

  # SCHAR_r
  y.f_SCHAR_r = xts::xts(seq(1,1, length.out = dim(y.r)[1]), order.by = zoo::index(y.r))
  for (i in 1:n_windows) {
    response = data[i:(nr-n_windows+i-1), 1, drop = FALSE]
    covariates = data[i:(nr-n_windows+i-1), c('P_d', 'P_w', 'N_d', 'N_w', 'M_d', 'M_m'), drop = FALSE]
    # covariates = data[i:(nr-n_windows+i-1), c('N_d', 'N_w', 'M_d', 'M_m'), drop = FALSE]
    fit <- fit_predict(response, covariates)
    y.f_SCHAR_r[i,1] <- fit

  }


  ls <- list(y.r = y.r, HAR_forecasts = y.f_HAR, SHAR_forecasts = y.f_SHAR, SCHAR_forecasts = y.f_SCHAR,
             SCHAR_r_forecasts = y.f_SCHAR_r)
  ls

}

#' Model confidence set Analysis
#'
#' Return the Superior Set of Models.
#'
#' @param ls A list containing the one step ahead forecasts
#' . Default = NULL.
#'
#' @param alpha a scalar in (0,1) indicating the confidence level of the tests.
#'
#' @return
#' A SSM object containing the Superior Set of Models.
#'
#' @examples
#' # Realized measures
#' realized1 <- drvp(data = xts_data) # realized variances
#' realized2 <- drsvp(data = xts_data) # realized semivariances
#' realized <- xts::merge.xts(realized1, realized2, join = 'inner')
#'
#' # Generate a frame to run HAR
#' frame <- HAR_frame(ts = realized)
#'
#' # Rolling window Analysis
#' forecasts <- rollwin(data = frame, length_training_data = 0.9)
#'
#' # MCS
#' MCS <- MCS(ls = forecasts, alpha = 0.05)
#'
#' @export

MCS <- function(ls = NULL, alpha = NULL) {

  realized <- ls[[1]]
  Loss <- do.call(cbind, lapply(names(ls)[-1],
              function(s) MCS::LossVol(realized,ls[[s]], which = "SE1")))
  colnames(Loss) <- names(ls)[-1]
  MCS_ <-  MCS::MCSprocedure(Loss = Loss, alpha = alpha ,B = 10000, statistic = "Tmax")
  MCS_
}


# # Realized measures
# realized1 <- drvp(data = xts_data) # realized variances
# realized2 <- drsvp(data = xts_data) # realized semivariances
# realized <- xts::merge.xts(realized1, realized2, join = 'inner')
#
# # Generate a frame to run HAR
# frame <- HAR_frame(ts = realized)
#
# # Rolling window Analysis
# forecasts <- rollwin(data = frame, length_training_data = 0.9)
#
# performance <- lapply(names(forecasts)[-1], function(x) {
#   list(h1=c((sum((forecasts[[1]]-forecasts[[x]])^2))/length(forecasts[[1]]),
#             (sum(abs(forecasts[[1]]-forecasts[[x]])))/length(forecasts[[1]])))
# })
#
#
# names(performance) <- names(forecasts)[-1]
#
# result <- data.frame(matrix(unlist(performance), nrow=length(performance), byrow=T))
