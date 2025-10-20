#' Make a forest plot
#'
#' Create a forest plot (intentionally spelled as provided) from a meta-analytic
#' summary from psychmeta
#'
#' @param meta_obj A barebones meta-analysis object from psychmeta
#' @return A list of plots made using ggplot
#' @export
#'
#' @examples
#' # make_forest_plot(meta_obj)
make_forest_plot <- function(meta_obj){
  meta_table <- psychmeta::get_metatab(meta_obj)
  num_plots = dim(meta_table)[1]
  col_number_analysis_type <- which(names(meta_table) == "k")
  col_number_moderator <- col_number_analysis_type - 1
  mod_col_name = names(meta_table)[col_number_moderator]

  overall_meta_tibble <- tibble::as_tibble(meta_table)
  if (num_plots> 1) {
    overall_meta_tibble <- overall_meta_tibble[seq(from = 1, to = nrow(overall_meta_tibble)), c(6,9,17,18)]
  } else {
    overall_meta_tibble <- overall_meta_tibble[seq(from = 1, to = nrow(overall_meta_tibble)), c(5,8,16,17)]
  }

  names(overall_meta_tibble) <- c("sample_id", "rxy", "lower_conf_limit_r", "upper_conf_limit_r")
  overall_meta_tibble$moderator <- "Overall"

  if (num_plots>2) {
    overall_meta_tibble$ypos_value <- c(1,seq(from = nrow(overall_meta_tibble), to = 2, by = -1))
  }


  meta_info <- psychmeta::get_escalc(meta_obj)

  plot_list = list()

  for (i in 1:num_plots) {
    mean_r <- meta_table$mean_r[i]
    table_name <- meta_table$analysis_id[i]
    if (num_plots > 1) {
      analysis_name <- as.character(dplyr::pull(meta_table[i,col_number_moderator]))
    } else {
      analysis_name = ""
    }

    meta_data <- meta_info[[i]]$barebones
    plot_data <- meta_data |>
      dplyr::select(
        sample_id,
        rxy,
        n,
        !!rlang::sym(names(meta_data)[3]))

    plot_data <- add_ci_r(plot_data)

    col_names_plot_data <- names(plot_data)
    col_names_plot_data[4] <- "moderator"
    names(plot_data) <- col_names_plot_data

    if (num_plots == 1) {
      plot_data$moderator = as.factor("Studies")
    }

    if (i != 1) {
      overall_plot_meta_tibble <- overall_meta_tibble[i,]
    } else {
      overall_plot_meta_tibble <- overall_meta_tibble
    }

    p = make_single_forest_plot(
      plot_data,
      analysis_name = analysis_name,
      overall_meta_tibble = overall_plot_meta_tibble
    )
    plot_list[[meta_table$analysis_id[i]]] = p
  }

  return(plot_list)

}

make_single_forest_plot <- function(plot_data, analysis_name, overall_meta_tibble) {
  # --- Combine data and prepare facets ---
  plot_data$ypos_value <- seq_len(nrow(plot_data)) + nrow(overall_meta_tibble)

  if (nrow(overall_meta_tibble) == 1) {
    overall_meta_tibble$ypos_value = 1
  }

  plot_data <- dplyr::bind_rows(plot_data, overall_meta_tibble) |>
    dplyr::mutate(
      moderator   = forcats::fct_relevel(moderator, "Overall", after = Inf),
      point_shape = dplyr::if_else(moderator == "Overall", 23, 16)  # diamond for Overall
    )

  min_r_plot <- min(plot_data$lower_conf_limit_r, na.rm = TRUE)

  # --- Build ggplot ---
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = rxy, y = ypos_value, label = sample_id)) +
    # Sample labels
    ggplot2::geom_text(ggplot2::aes(x = min_r_plot - .5), hjust = 0) +

    # Points: black diamonds for Overall, black circles for others
    ggplot2::geom_point(ggplot2::aes(shape = point_shape), fill = "black", size = 3) +
    ggplot2::scale_shape_identity() +

    # --- Error bars for Overall (always black) ---
    ggplot2::geom_errorbar(
      data = dplyr::filter(plot_data, moderator == "Overall"),
      ggplot2::aes(
        y = ypos_value,
        xmin = lower_conf_limit_r,
        xmax = upper_conf_limit_r
      ),
      color = "black",
      width = 0.2,
      orientation = "y"
    ) +

    # --- Error bars for other facets (default color palette) ---
    ggplot2::geom_errorbar(
      data = dplyr::filter(plot_data, moderator != "Overall"),
      ggplot2::aes(
        y = ypos_value,
        xmin = lower_conf_limit_r,
        xmax = upper_conf_limit_r,
        color = moderator
      ),
      width = 0.2,
      orientation = "y"
    ) +

    # Labels and layout
    ggplot2::labs(
      x = "Observed Correlation",
      y = "",
      title = paste("Forest Plot:", analysis_name)
    ) +

    ggplot2::facet_grid(rows = ggplot2::vars(moderator), scales = "free_y") +

    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "black", color = "black"),
      strip.text       = ggplot2::element_text(color = "white", face = "bold"),
      axis.ticks.y     = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_blank(),
      legend.position  = "none"
    )

  # --- Force "Overall" facet to 15% of total height ---
  n_panels <- nlevels(plot_data$moderator)
  row_heights <- if (n_panels >= 2) {
    c(rep(0.85 / (n_panels - 1), n_panels - 1), 0.15)
  } else 1

  p + ggh4x::force_panelsizes(
    rows = grid::unit(row_heights, "null"),
    cols = grid::unit(1, "null")
  )
}
