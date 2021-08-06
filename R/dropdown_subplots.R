# Subplots ---------------------------------------------------------------------
make_subplots_dropdowns <- function(
  data, group_column, nrows = 2, margin = 0.07, shareY = TRUE, 
  shareX = TRUE, titleX = shareX, master_y_axis_title = NULL,
  showlegend = TRUE, ...){
  
  group_column_plot <- {{ group_column }}
  group_column <- formula_to_sym(group_column)
  
  plots <- data %>%
    mutate(
      showlegend = if_else({{ group_column }} == {{ group_column }}[1], showlegend, FALSE),
      group = paste0("<b>", {{ group_column }}, "</b>")
    ) %>%
    group_nest({{ group_column }}, keep = TRUE) %>%
    mutate(
      plots = map(
        .x = data,
        .f = function(.x, nrows, ...){
          plot <- dropdown_plots(
            data = .x,
            ...,
            showlegend = unique(.x$showlegend)
          ) %>%
            add_annotations(
              text = unique(.x$group),
              showarrow = FALSE,
              x = -0.05,
              y = 1.1,
              xref = "paper",
              yref = "paper",
              xanchor = "left",
              yanchor = "bottom"
            )
          
          plot
        },
        nrows = nrows,
        ...
      )
    )
  
  if (nrows > 1) {
    # correct heights to account for margin
    heights_1 <- (nrows - (1 + 2*margin)*(nrows - 2))
    heights_2 <- rep(1 + 2*margin, nrows - 2)
    heights <- c(heights_1/2, heights_2, heights_1/2)
  } else {
    # for 1 row no need for heights
    heights <- 1
  }
  
  # subplot can take our fancy, nested data-frame as an argument
  plots <- plots %>%
    subplot(
      nrows = nrows,
      heights = heights/nrows,
      shareX = shareX,
      shareY = shareY,
      titleX = titleX,
      margin = margin
    ) %>%
    layout(
      margin = list(
        t = 60
      )
    )
  
  
  plots
}
