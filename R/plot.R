#' Generate linkage bubble plot by sector category
#'
#' Scatter plot of ITBL (Indeks Daya Penyebaran) against ITFL (Indeks Derajat
#' Kepekaan), with bubble size showing combined linkage strength and colour
#' showing sector category. Quadrant reference lines are drawn at 1.
#'
#' Use [generate_quadrant_plot()] for the companion view coloured by BPS
#' quadrant instead of category.
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#' @param show_labels Logical. If `TRUE`, sector IDs are displayed.
#' @param base_size Numeric. Base font size for ggplot.
#'
#' @return A `ggplot` object.
#'
#' @export
generate_prs_plot <- function(linkages_table, show_labels = TRUE, base_size = 12) {
  # ITBL and ITFL are normalised so their mean is exactly 1: the reference line
  # at 1 is therefore both the BPS threshold and the cross-sector average.
  ref <- 1
  linkages_table$Bubble_Size <- linkages_table$ITBL + linkages_table$ITFL
  x_min <- min(linkages_table$ITBL, na.rm = TRUE)
  x_max <- max(linkages_table$ITBL, na.rm = TRUE)
  y_min <- min(linkages_table$ITFL, na.rm = TRUE)
  y_max <- max(linkages_table$ITFL, na.rm = TRUE)

  p <- ggplot(linkages_table, aes(x = ITBL, y = ITFL, size = Bubble_Size, color = CATEGORY)) +
    geom_point(alpha = 0.8, aes(text = paste0("SECTOR: ", SECTOR))) +
    geom_hline(yintercept = ref, colour = "#BB0000", linetype = "dashed") +
    geom_vline(xintercept = ref, colour = "#BB0000", linetype = "dashed") +
    # Annotations sit in the plot corners: the region midpoints fall inside the
    # dense cluster of sectors around the reference lines.
    annotate("text", x = x_max, y = y_max, hjust = 1, vjust = 1,
             label = "Kuadran I\Sektor unggulan",
             fontface = "bold", size = 4, colour = "grey30") +
    annotate("text", x = x_max, y = y_min, hjust = 1, vjust = 0,
             label = "Kuadran II\nPotensi unggul",
             fontface = "bold", size = 4, colour = "grey30") +
    annotate("text", x = x_min, y = y_max, hjust = 0, vjust = 1,
             label = "Kuadran III\nKurang diunggulkan",
             fontface = "bold", size = 4, colour = "grey30") +
    annotate("text", x = x_min, y = y_min, hjust = 0, vjust = 0,
             label = "Kuadran IV\nTertinggal",
             fontface = "bold", size = 4, colour = "grey30") +
    labs(x = "Indeks Daya Penyebaran (ITBL)",
         y = "Indeks Derajat Kepekaan (ITFL)",
         color = "Sector Category") +
    scale_size_continuous(range = c(4, 18), guide = "none") +
    theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")

  if (show_labels) {
    p <- p + geom_text(aes(label = ID), size = 3, color = "black", vjust = -1)
  }

  p
}

#' Generate top-N multiplier bar chart
#'
#' Horizontal bar chart of the N sectors with the highest value of one
#' multiplier, with the value printed at the end of each bar.
#'
#' @param multiplier_table A tibble produced by [combine_multipliers()].
#' @param column Character. Column to plot, e.g. `"Out.multiplier"`.
#' @param title Character. Plot title.
#' @param x_label Character. Axis label for the multiplier value.
#' @param top_n Integer. Number of sectors to show (default 15).
#' @param bar_colour Character. Fill colour.
#' @param digits Integer. Rounding for the printed value.
#'
#' @return A `ggplot` object.
#'
#' @export
generate_multiplier_bar_chart <- function(multiplier_table,
                                          column,
                                          title,
                                          x_label = "Multiplier",
                                          top_n = 15,
                                          bar_colour = "#FFA61A",
                                          digits = 4) {
  if (!column %in% names(multiplier_table))
    stop(sprintf("Column '%s' not found in multiplier_table.", column), call. = FALSE)

  df <- as.data.frame(multiplier_table)
  df$VALUE <- as.numeric(df[[column]])
  df <- df[!is.na(df$VALUE), ]
  df <- head(df[order(-df$VALUE), ], top_n)
  df$LABEL <- format(round(df$VALUE, digits), trim = TRUE)

  ggplot(df, aes(x = stats::reorder(SECTOR, VALUE), y = VALUE,
                 text = paste0(SECTOR, "<br>", x_label, ": ", LABEL))) +
    geom_col(fill = bar_colour) +
    geom_text(aes(label = LABEL), hjust = -0.15, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = title, x = NULL, y = x_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(colour = bar_colour, face = "bold", hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

#' Generate GDP bar chart
#'
#' Creates a horizontal bar chart for the top sectors by GDP.
#'
#' @param gdp_sector A tibble produced by [calculate_gdp()].
#' @param sector_info A tibble with sector names and categories.
#' @param top_n Integer. Number of top sectors to display.
#'
#' @return A list with elements `plot` (ggplot) and `data` (tibble).
#'
#' @export
generate_gdp_bar_chart <- function(gdp_sector, sector_info, top_n = 10) {
  plot_df <- merge(gdp_sector, sector_info,
                   by.x = "SECTOR", by.y = colnames(sector_info)[1], all.x = TRUE)
  total_gdp <- sum(plot_df$GDP, na.rm = TRUE)
  plot_df$PERCENTAGE <- round(100 * plot_df$GDP / total_gdp, 2)
  plot_df <- head(plot_df[order(-plot_df$GDP), ], top_n)
  category_col <- colnames(sector_info)[2]
  
  p <- ggplot(plot_df, aes(x = reorder(SECTOR, GDP), y = GDP,
                           fill = .data[[category_col]],
                           text = paste0("<b>Sector:</b> ", SECTOR,
                                         "<br><b>GDP:</b> ",
                                         format(round(GDP,0), big.mark = ","),
                                         " Million IDR",
                                         "<br><b>Contribution:</b> ", PERCENTAGE, "%"))) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "GDP (Million IDR)", fill = "Category") +
    theme_minimal()
  
  list(plot = p, data = plot_df)
}

#' Generate GDP donut chart
#'
#' Creates an interactive donut chart showing the share of top sectors in
#' total GDP.
#'
#' @param gdp_sector A tibble produced by [calculate_gdp()].
#' @param sector_info A tibble with sector names and categories.
#' @param top_n Integer. Number of top sectors to display.
#'
#' @return A list with elements `plot` (plotly) and `data` (tibble).
#'
#' @export
generate_gdp_donut_chart <- function(gdp_sector, sector_info, top_n = 10) {
  plot_df <- merge(gdp_sector, sector_info,
                   by.x = "SECTOR", by.y = colnames(sector_info)[1], all.x = TRUE)
  total_gdp <- sum(plot_df$GDP, na.rm = TRUE)
  plot_df$CONTRIBUTION <- round(100 * plot_df$GDP / total_gdp, 2)
  plot_df <- head(plot_df[order(-plot_df$GDP), ], top_n)
  plot_df$SECTOR_LABEL <- stringr::str_wrap(plot_df$SECTOR, width = 25)
  
  p <- plot_ly(data = plot_df, labels = ~SECTOR_LABEL, values = ~GDP,
               type = "pie", hole = 0.55,
               text = ~paste0(round(CONTRIBUTION, 1), "%"),
               textinfo = "label+text",
               hovertemplate = paste("<b>%{label}</b><br>GDP: %{value:,.0f} Million IDR",
                                     "<br>Contribution to Total GDP: %{customdata:.2f}%",
                                     "<extra></extra>")) %>%
    layout(title = "Top 10 GDP Contribution by Sector", showlegend = FALSE)
  
  list(plot = p, data = plot_df)
}

#' Plot comparison of scenarios
#'
#' Creates a line plot comparing a chosen indicator across scenarios over
#' the projection years. If only one scenario is provided, a single line is
#' drawn.
#'
#' @param data A data frame with columns `YEAR`, `SCENARIO`, and the
#'   indicator column specified in `value_col`.
#' @param value_col Character. Name of the indicator column to plot.
#' @param title Character. Plot title.
#' @param y_label Character. Label for the y-axis.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_comparison <- function(data, value_col, title, y_label) {
  n_scenarios <- length(unique(data$SCENARIO))
  
  if (n_scenarios > 1) {
    ggplot(data, aes(x = YEAR, y = .data[[value_col]], color = SCENARIO)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = unique(data$YEAR)) +
      scale_color_brewer(palette = "Set1") +
      labs(title = title, x = "Tahun", y = y_label, color = "Skenario") +
      theme_minimal()
  } else {
    ggplot(data, aes(x = YEAR, y = .data[[value_col]])) +
      geom_line(linewidth = 1, color = "#4472C4") +
      geom_point(size = 3, color = "#4472C4") +
      scale_x_continuous(breaks = unique(data$YEAR)) +
      labs(title = title, x = "Tahun", y = y_label) +
      theme_minimal()
  }
}