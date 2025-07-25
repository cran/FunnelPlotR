#' Overdispersion-adjusted funnel limit calculation
#'
#' @description Add 95% and 99.8 % funnel limits from OD-adjusted Poisson
#' distribution
#'
#' @param df_ci Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly
#' Standardised ratio ("SR"), proportion ("PR")
#' , or ratio of counts ("RC").
#' @param sr_method Adjustment method for standardised ratios, can take the
#' value "SHMI" or "CQC". "SHMI" is default.
#' @param multiplier Multiplier to adjust limits if reporting by a multiplier
#' , e.g. per 1000.
#' @param tau2 A 'between' standard deviation to add to the within standard
#' deviation, S, to inflate limits.
#' @param target The centre line of the plot. Mean for non-SRs or 1 for SR
#' @keywords internal
#' @return A data.frame of with appended OD limits
calculate_limits <- function(df_ci = df_ci
                             , data_type = "SR"
                             , sr_method = "SHMI"
                             , multiplier = 1
                             , tau2 = 0
                             , target = target
                             , draw_adjusted = draw_adjusted) {

  # Construct variable names for limits
  od <- ifelse(draw_adjusted == TRUE, "od", "")
  var_names <- paste0(od, c("ll95", "ul95", "ll998", "ul998"))

  # Set between-institution variance to zero for calculating approximate
  #   unadjusted limits for PR and RC data
  tau2 <- ifelse(draw_adjusted == FALSE, 0, tau2)

  if (data_type == "SR") {
    if (draw_adjusted == FALSE) {
      # Identify number of existing columns, appending calc limits at end
      ncols <- ncol(df_ci)

      df_ci[, ncols + 1] <- multiplier * target *
        (qchisq(0.975, 2 * df_ci$number_seq, lower.tail = FALSE) / 2) /
        df_ci$number_seq

      df_ci[, ncols + 2] <- multiplier * target *
        (qchisq(0.025, 2 * (df_ci$number_seq + 1), lower.tail = FALSE) / 2) /
        df_ci$number_seq

      df_ci[, ncols + 3] <- multiplier * target *
        (qchisq(0.999, 2 * df_ci$number_seq, lower.tail = FALSE) / 2) /
        df_ci$number_seq

      df_ci[, ncols + 4] <- multiplier * target *
        (qchisq(0.001, 2 * (df_ci$number_seq + 1), lower.tail = FALSE) / 2) /
        df_ci$number_seq

    } else if (sr_method == "SHMI") {
      df_ci$s <- sqrt(1 / df_ci$number_seq)
      ncols <- ncol(df_ci)

      df_ci[, ncols + 1] <- multiplier *
        (exp(-1.959964 * sqrt((1 / df_ci$number_seq) + tau2)))

      df_ci[, ncols + 2] <- multiplier *
        (exp(1.959964  * sqrt((1 / df_ci$number_seq) + tau2)))

      df_ci[, ncols + 3] <- multiplier *
        (exp(-3.090232 * sqrt((1 / df_ci$number_seq) + tau2)))

      df_ci[, ncols + 4] <- multiplier *
        (exp(3.090232  * sqrt((1 / df_ci$number_seq) + tau2)))

    } else {

      df_ci$s <- 1 / (2 * sqrt(df_ci$number_seq))

      ncols <- ncol(df_ci)

      df_ci[, ncols + 1] <- multiplier *
        (1 - (1.959964 * sqrt((df_ci$s^2 + tau2))))
      df_ci[, ncols + 2] <- multiplier *
        (1 + (1.959964 * sqrt((df_ci$s^2 + tau2))))
      df_ci[, ncols + 3] <- multiplier *
        (1 - (3.090232 * sqrt((df_ci$s^2 + tau2))))
      df_ci[, ncols + 4] <- multiplier *
        (1 + (3.090232 * sqrt((df_ci$s^2 + tau2))))
    }

  } else if (data_type == "RC") {
    df_ci$s <- 1 / (2 * sqrt(df_ci$number_seq))
    ncols <- ncol(df_ci)

    df_ci[, ncols + 1] <- multiplier *
      (exp(log(target) - (1.959964 * sqrt(df_ci$s^2 + tau2))))

    df_ci[, ncols + 2] <- multiplier *
      (exp(log(target) + (1.959964 * sqrt(df_ci$s^2 + tau2))))

    df_ci[, ncols + 3] <- multiplier *
      (exp(log(target) - (3.090232 * sqrt(df_ci$s^2 + tau2))))

    df_ci[, ncols + 4] <- multiplier *
      (exp(log(target) + (3.090232 * sqrt(df_ci$s^2 + tau2))))

  } else if (data_type == "PR") {
    df_ci$s <- 1 / (2 * sqrt(df_ci$number_seq))
    ncols <- ncol(df_ci)

    df_ci[, ncols + 1] <-  multiplier *
      (sin(asin(sqrt(target)) - 1.959964 * sqrt((df_ci$s^2) + tau2))^2)

    df_ci[, ncols + 2] <-  multiplier *
      (sin(asin(sqrt(target)) + 1.959964 * sqrt((df_ci$s^2) + tau2))^2)

    df_ci[, ncols + 3] <-  multiplier *
      (sin(asin(sqrt(target)) - 3.090232 * sqrt((df_ci$s^2) + tau2))^2)

    df_ci[, ncols + 4] <-  multiplier *
      (sin(asin(sqrt(target)) + 3.090232 * sqrt((df_ci$s^2) + tau2))^2)

    # Truncate proportion limits at [0, 1]
    df_ci[, ncols + 1] <-  ifelse(df_ci[, ncols + 1] < 0, 0, df_ci[, ncols + 1])

    df_ci[, ncols + 2] <-  ifelse(df_ci[, ncols + 2] > 1 * multiplier
                                  , 1 * multiplier, df_ci[, ncols + 2])

    df_ci[, ncols + 3] <-  ifelse(df_ci[, ncols + 3] < 0, 0, df_ci[, ncols + 3])

    df_ci[, ncols + 4] <-  ifelse(df_ci[, ncols + 4] > 1 * multiplier
                                  , 1 * multiplier, df_ci[, ncols + 4])

    # Arcsine trans at low denominators may not be monotonic, truncate if occurs
    df_ci[1:(nrow(df_ci) - 1), ncols + 1] <-
      ifelse(df_ci[1:(nrow(df_ci) - 1), ncols + 1] >
               df_ci[2:(nrow(df_ci)), ncols + 1],
             0, df_ci[1:(nrow(df_ci) - 1), ncols + 1])

    df_ci[1:(nrow(df_ci) - 1), ncols + 2] <-
      ifelse(df_ci[1:(nrow(df_ci) - 1), ncols + 2] <
               df_ci[2:(nrow(df_ci)), ncols + 2],
             1 * multiplier, df_ci[1:(nrow(df_ci) - 1), ncols + 2])

    df_ci[1:(nrow(df_ci) - 1), ncols + 3] <-
      ifelse(df_ci[1:(nrow(df_ci) - 1), ncols + 3] >
               df_ci[2:(nrow(df_ci)), ncols + 3],
             0, df_ci[1:(nrow(df_ci) - 1), ncols + 3])

    df_ci[1:(nrow(df_ci) - 1), ncols + 4] <-
      ifelse(df_ci[1:(nrow(df_ci) - 1), ncols + 4] <
               df_ci[2:(nrow(df_ci)), ncols + 4],
             1 * multiplier, df_ci[1:(nrow(df_ci) - 1), ncols + 4])
  }

  # Add variable names
  colnames(df_ci) <- c(colnames(df_ci)[1:ncols], var_names)

  df_ci
}
