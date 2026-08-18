#' Generate quadrant scatter plot
#'
#' Creates a scatter plot of Direct Backward Linkage (DBL) vs Direct Forward
#' Linkage (DFL) with quadrant lines.
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#' @param show_labels Logical. If `TRUE`, sector IDs are displayed.
#' @param base_size Numeric. Base font size for ggplot.
#'
#' @return A `ggplot` object.
#'
#' @export
generate_prs_plot <- function(linkages_table, show_labels = TRUE, base_size = 12) {
  avg_dbl <- 1
  avg_dfl <- 1
  linkages_table$Bubble_Size <- linkages_table$DBL + linkages_table$DFL
  x_min <- min(linkages_table$DBL, na.rm = TRUE)
  x_max <- max(linkages_table$DBL, na.rm = TRUE)
  y_min <- min(linkages_table$DFL, na.rm = TRUE)
  y_max <- max(linkages_table$DFL, na.rm = TRUE)
  
  p <- ggplot(linkages_table, aes(x = DBL, y = DFL, size = Bubble_Size, color = CATEGORY)) +
    geom_point(alpha = 0.8, aes(text = paste0("SECTOR: ", SECTOR))) +
    geom_hline(yintercept = avg_dfl, colour = "#BB0000", linetype = "dashed") +
    geom_vline(xintercept = avg_dbl, colour = "#BB0000", linetype = "dashed") +
    annotate("text", x = (avg_dbl + x_max)/2, y = (avg_dfl + y_max)/2,
             label = "Quadrant I\nHigh DBL - High DFL", fontface = "bold", size = 4) +
    annotate("text", x = (x_min + avg_dbl)/2, y = (avg_dfl + y_max)/2,
             label = "Quadrant II\nLow DBL - High DFL", fontface = "bold", size = 4) +
    annotate("text", x = (x_min + avg_dbl)/2, y = (y_min + avg_dfl)/2,
             label = "Quadrant III\nLow DBL - Low DFL", fontface = "bold", size = 4) +
    annotate("text", x = (avg_dbl + x_max)/2, y = (y_min + avg_dfl)/2,
             label = "Quadrant IV\nHigh DBL - Low DFL", fontface = "bold", size = 4) +
    labs(x = "Direct Backward Linkage (DBL)",
         y = "Direct Forward Linkage (DFL)",
         color = "Sector Category") +
    scale_size_continuous(range = c(4, 18), guide = "none") +
    theme_minimal(base_size = base_size) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  
  if (show_labels) {
    p <- p + geom_text(aes(label = ID), size = 3, color = "black", vjust = -1)
  }
  
  p
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