#' @title Label outliers
#'
#' @param mod_plot_agg Aggregated data set for plotting
#' @param draw_adjusted Logical for drawing OD limits, takes precedence
#'  over Poisson for outliers
#' @param draw_unadjusted Logical for drawing unadjusted limits
#' @param limit which limit to use.  Currently 95 or 99.
#' @param multiplier how much to multiply the output by.
#'
#' @keywords internal
#' @noRd
outliers_func <- function(mod_plot_agg, draw_adjusted, draw_unadjusted
                          , limit, multiplier) {
  if (draw_adjusted == FALSE && draw_unadjusted == FALSE) {
    mod_plot_agg$outlier <- 0
  } else {
    if (limit == 95) {
      if (draw_adjusted == FALSE) {
        mod_plot_agg$outlier <-
          ifelse((mod_plot_agg$rr * multiplier) <= mod_plot_agg$UCL95 &
                   (mod_plot_agg$rr  * multiplier) >= mod_plot_agg$LCL95,
                 0,
                 1)
      } else {
        mod_plot_agg$outlier <-
          ifelse((mod_plot_agg$rr * multiplier) <= mod_plot_agg$OD95UCL &
                   (mod_plot_agg$rr  * multiplier) >= mod_plot_agg$OD95LCL,
                 0,
                 1)
      }
    } else {

      if (limit == 99) {

        if (draw_adjusted == FALSE){
          mod_plot_agg$outlier <-
            ifelse((mod_plot_agg$rr * multiplier) <= mod_plot_agg$UCL99 &
                     (mod_plot_agg$rr * multiplier) >= mod_plot_agg$LCL99,
                   0,
                   1)
        } else {
          mod_plot_agg$outlier <-
            ifelse((mod_plot_agg$rr * multiplier) <= mod_plot_agg$OD99UCL &
                     (mod_plot_agg$rr * multiplier) >= mod_plot_agg$OD99LCL,
                   0,
                   1)
        }
      }
    }
  }
  mod_plot_agg
}
