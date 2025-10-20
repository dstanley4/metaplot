#' Make a funnel plot
#'
#' Create a funnel plot (intentionally spelled as provided) from a meta-analytic
#' summary from psychmeta
#' @param meta_obj A barebones meta-analysis object from psychmeta
#' @param sig_level .05
#' @param show_sig_level_lines TRUE
#' @param show_expected_dist_lines TRUE
#' @param sig_label "Region of non-significance
#' @param expected_label = "Expected random sampling range (Assuming no moderators)"
#' @param sig_color = "firebrick"
#' @param expected_color = "blue"
#' @param legend_title = "Interpretation of Lines"
#' @param legend_position = "right"
#'
#' @return A list of plots made using ggplot
#' @export
#'
#' @examples
#' # make_funnel_plot(meta_obj)
make_funnel_plot <- function(meta_obj,
                             sig_level = 0.05,
                             show_sig_level_lines = TRUE,
                             show_expected_dist_lines = TRUE,
                             sig_label = "Region of non-significance",
                             expected_label = "Expected random sampling range\n(Assuming no moderators)",
                             sig_color = "firebrick",
                             expected_color = "blue",
                             legend_title = "Interpretation of Lines",
                             legend_position = "right") {

  meta_table <- psychmeta::get_metatab(meta_obj)
  num_plots = dim(meta_table)[1]
  col_number_analysis_type <- which(names(meta_table) == "k")
  col_number_moderator <- col_number_analysis_type - 1
  mod_col_name = names(meta_table)[col_number_moderator]




  meta_info <- psychmeta::get_escalc(meta_obj)

  plot_list = list()

  for (i in 1:num_plots) {
    mean_r <- meta_table$mean_r[i]
    table_name <- meta_table$analysis_id[i]
    analysis_name <- as.character(dplyr::pull(meta_table[i,col_number_moderator]))
    if (i ==1) {
      analysis_name <- paste(analysis_name, mod_col_name, sep = " of ")
    } else {
      analysis_name <- paste(analysis_name, mod_col_name, sep = " level of ")
    }

    meta_data <- meta_info[[i]]$barebones
    observed_variance <- meta_obj$meta_tables[[i]]$barebones$var_r
    expected_variance <- meta_obj$meta_tables[[i]]$barebones$var_e
    percent_sampling_error <- expected_variance / observed_variance * 100


    p = make_single_funnel_plot(
      meta_data,
      sig_level = sig_level,
      show_sig_level_lines = show_sig_level_lines,
      show_expected_dist_lines = show_expected_dist_lines,
      sig_label = sig_label,
      expected_label = expected_label,
      sig_color = sig_color,
      expected_color = expected_color,
      legend_title = legend_title,
      legend_position = legend_position,
      percent_sampling_error = percent_sampling_error,
      mean_r = mean_r,
      analysis_name = analysis_name
    )
    plot_list[[meta_table$analysis_id[i]]] = p
  }

  return(plot_list)
}


make_single_funnel_plot <- function(meta_data,
                                    sig_level = 0.05,
                                    show_sig_level_lines = TRUE,
                                    show_expected_dist_lines = TRUE,
                                    sig_label = "Region of non-significance",
                                    expected_label = "Expected random sampling range\n(Assuming no moderators)",
                                    sig_color = "firebrick",
                                    expected_color = "blue",
                                    legend_title = "Interpretation of Lines",
                                    legend_position = "right",
                                    percent_sampling_error = NULL,
                                    mean_r = mean_r,
                                    analysis_name) {


  ns_min <- base::min(meta_data$n, na.rm = TRUE)
  ns_max <- base::max(meta_data$n, na.rm = TRUE)
  nrange <- c(ns_min, ns_max)

  df_sig <- if (show_sig_level_lines) {
    get_dist_r_range(n = nrange, sig_level = sig_level, rho = 0) |>
      dplyr::mutate(type = sig_label)
  } else NULL

  df_meta <- if (show_expected_dist_lines) {
    get_dist_r_range(n = nrange, sig_level = sig_level, rho = mean_r) |>
      dplyr::mutate(type = expected_label)
  } else NULL

  df_lines <- dplyr::bind_rows(df_sig, df_meta)
  if (!is.null(df_lines) && base::nrow(df_lines) > 0) {
    df_lines <- df_lines |>
      tidyr::pivot_longer(cols = c(LL, UL), names_to = "side", values_to = "r") |>
      dplyr::arrange(type, side, n)
  }

  title_str <- base::sprintf(
    "Funnel Plot: %s",
    analysis_name
  )

  subtitle_str <- base::sprintf(
    "%.0f%% of observed correlation variance due to sampling error\nSignificance level: alpha = %.3f",
    percent_sampling_error, sig_level
  )

  p <- ggplot2::ggplot(meta_data, ggplot2::aes(x = rxy, y = n)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::labs(
      x = "Sample correlation (r)",
      y = "Sample size (n)",
      title = title_str,
      subtitle = subtitle_str
    ) +
    ggplot2::theme_classic()

  if (!is.null(df_lines) && base::nrow(df_lines) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = df_lines,
        ggplot2::aes(
          x = r, y = n,
          color = type, linetype = type,
          group = base::interaction(type, side)
        ),
        linewidth = 0.9,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_color_manual(
        name = legend_title,
        values = stats::setNames(
          c(expected_color, sig_color),
          c(expected_label, sig_label)
        )
      ) +
      ggplot2::scale_linetype_manual(
        name = legend_title,
        values = stats::setNames(
          c("solid", "dashed"),
          c(expected_label, sig_label)
        )
      ) +
      ggplot2::theme(
        legend.position = legend_position,
        legend.title = ggplot2::element_text(face = "bold")
      )
  }

  p
}


