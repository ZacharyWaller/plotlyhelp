# Subplots ---------------------------------------------------------------------
make_subplots <- function(data, group_column, nrows = 2, margin = 0.07, 
                          shareY = TRUE, shareX = TRUE, plot_title = NULL, y_axis_title = NULL, ...){
  #' Title
  #'
  #' @param data 
  #' @param group_column 
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
  group_column_plot <- {{ group_column }}
  group_column <- formula_to_sym(group_column)
  
  dots <- list(...)
  
  # one y-axis range for all plots
  y_min <- data %>%
    pull(formula_to_sym(dots$y)) %>%
    min(na.rm = TRUE)
  
  y_min <- min(0, y_min)
  
  y_max <- data %>%
    pull(formula_to_sym(dots$y)) %>%
    max(na.rm = TRUE)
  
  y_axis_range <- c(y_min, y_max*1.1)
  
  groups <- unique(pull(data, {{ group_column }}))
  
  plot_title_ls <- vector(mode = "list", length = length(groups))
  if (!is.null(plot_title)) {
    
    plot_title_ls[1] <- plot_title 
    
  }
  
  y_axis_title_ls <- paste(groups, y_axis_title, sep = "\n")
  
  
  plots <- data %>%
    mutate(
      showlegend = if_else({{ group_column }} == {{ group_column }}[1], TRUE, FALSE),
      group = paste0("<b>", {{ group_column }}, "</b>")
    ) %>%
    group_nest({{ group_column }}, keep = TRUE) %>%
    mutate(
      plots = pmap(
        .l = list(
          data = data,
          y_axis_title = y_axis_title_ls,
          plot_title = plot_title_ls
        ), 
        .f = function(data, y_axis_title, plot_title, nrows, ...){
          
          plot <- make_plot(
            data = data,
            y_axis_title = y_axis_title,
            plot_title = plot_title,
            ...,
            showlegend = unique(data$showlegend)
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
      titleY = TRUE,
      margin = margin
    ) %>%
    layout(
      margin = list(
        t = 100
      )
    )
  
  plots
}
