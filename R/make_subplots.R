# Subplots ---------------------------------------------------------------------
make_subplots <- function(data, x, y, subplot, breakdown = NULL, bar_line = NULL, nrows = 2, margin = 0.07, 
                          shareY = TRUE, shareX = TRUE, titleY = shareY, titleX = shareX,
                          plot_title = NULL, x_axis_title = NULL, y_axis_title = NULL, orientation = "v",
                          showlegend = FALSE, figure_name = NULL, heights = NULL, ...){
  #' Title
  #'
  #' @param data 
  #' @param subplot 
  #' @param nrows 
  #' @param margin 
  #' @param shareY 
  #' @param shareX 
  #' @param master_y_axis_title 
  #' @param ... 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' 
  
  # Save data ------------------------------------------------------------------
  if (!is.null(figure_name)) {
    save_data(data, x = x, y = y, breakdown = breakdown, bar_line = bar_line, subplot = subplot, figure_name = figure_name)
  }
  
  subplot_plot <- {{ subplot }}
  subplot <- formula_to_sym(subplot)
  
  groups <- unique(pull(data, {{ subplot }}))
  
  plot_title_ls <- vector(mode = "list", length = length(groups))
  if (!is.null(plot_title)) {
    
    plot_title_ls[1] <- plot_title 
    
  }
  
  # Add plot title to first plot in every dropdown option, but no other plots
  showlegend_df <- data %>%
    distinct(
      {{ subplot }}
    ) %>%
    mutate(
      showlegend = showlegend
    )
  
  plots <- data %>%
    mutate(
      group = paste0("<b>", {{ subplot }}, "</b>")
    ) %>%
    # add on plot title data
    left_join(
      showlegend_df, by = intersect(colnames(.), colnames(showlegend_df))
    ) %>%
    group_nest({{ subplot }}, keep = TRUE) %>%
    mutate(
      plots = pmap(
        .l = list(
          data = data,
          #y_axis_title = y_axis_title_ls,
          plot_title = plot_title_ls
        ), 
        .f = function(data, y_axis_title, plot_title, nrows, x, y, ...){
          
          if (orientation == "h") {
            x_axis_title <- paste0("<b>", unique(data$group), "</b>", "\n", x_axis_title)
          } else {
            y_axis_title <- paste0("<b>", unique(data$group), "</b>", "\n", y_axis_title)
          }
          
          plot <- make_plot(
            data = data,
            y_axis_title = y_axis_title,
            x_axis_title = x_axis_title,
            orientation = orientation,
            plot_title = plot_title,
            breakdown = breakdown, 
            bar_line = bar_line,
            x = x,
            y = y,
            ...,
            showlegend = unique(data$showlegend)
          )
          
          plot
        },
        nrows = nrows,
        y_axis_title = y_axis_title,
        x = x,
        y = y,
        ...
      )
    )
  
  # subplot can take our fancy, nested data-frame as an argument
  plots <- plots %>%
    subplot(
      nrows = nrows,
      shareX = shareX,
      shareY = shareY,
      titleY = titleY,
      titleX = titleX,
      margin = margin,
      heights = heights
    ) %>%
    layout(
      margin = list(
        t = 100,
        b = 100
      )
    )
  
  plots
}
