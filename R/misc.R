#' @noRd
# (optional) @importFrom utils globalVariables
utils::globalVariables(c(
  "LL", "UL", "ci", "lower_conf_limit_r", "moderator", "n",
  "point_shape", "r", "rxy", "sample_id", "side", "type",
  "upper_conf_limit_r", "ypos_value"
))


#' @noRd
ci_r <- function(r, n, conf.level = 0.95) {
  probability_in_tail <- (1 - conf.level)/2
  obs_z <- atanh(r)
  obs_z_se <- 1/sqrt(n - 3)
  high_z <- stats::qnorm(p = probability_in_tail, mean = obs_z,
                         sd = obs_z_se, lower.tail = FALSE)
  high_r <- tanh(high_z)
  low_z <- stats::qnorm(p = probability_in_tail, mean = obs_z,
                        sd = obs_z_se, lower.tail = TRUE)
  low_r <- tanh(low_z)
  conf_interval_output <- list()
  conf_interval_output$r <- r
  conf_interval_output$lower_conf_limit_r <- low_r
  conf_interval_output$upper_conf_limit_r <- high_r
  return(conf_interval_output)
}

#' @noRd
add_ci_r <- function(plot_data) {
  plot_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      ci = list(ci_r(rxy, n)),
      lower_conf_limit_r = ci$lower,
      upper_conf_limit_r = ci$upper
    ) |>
    dplyr::select(-ci) |>
    dplyr::ungroup()
}

#' @noRd
get_dist_r_range <- function(n, sig_level = 0.05, rho) {
  # n can be a single number or c(min, max)
  prob_tail <- 1 - sig_level / 2

  r_as_z <- base::atanh(rho)

  n_seq <- base::seq(base::min(n), base::max(n), by = 1)
  se <- 1 / base::sqrt(n_seq - 3)

  z_crit_upper <- stats::qnorm(p = prob_tail, mean = r_as_z, sd = se, lower.tail = TRUE)
  z_crit_lower <- stats::qnorm(p = sig_level, mean = r_as_z, sd = se, lower.tail = TRUE)

  r_crit_upper <- base::tanh(z_crit_upper)
  r_crit_lower <- base::tanh(z_crit_lower)

  # Return a tibble for tidyverse consistency
  tibble::tibble(n = n_seq, LL = r_crit_lower, UL = r_crit_upper)
}

