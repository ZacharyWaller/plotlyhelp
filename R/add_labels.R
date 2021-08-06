# Label lines ------------------------------------------------------------------
add_labels <- function(data, x, y, breakdown, gap_factor = 0.045){
  #' Generates text labels to go at the end of a line plot, one label per breakdown
  #' group, 
  #'
  #' @param data 
  #' @param x 
  #' @param y 
  #' @param breakdown 
  #' @param gap_factor 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  #' 
  #' 
  #' 

  
  x <- formula_to_sym(x)
  y <- formula_to_sym(y)
  breakdown <- formula_to_sym(breakdown)
  
  # check for numeric values
  x_class <- data %>%
    pull({{ x }}) %>%
    class()
  
  if (!x_class %in% c("numeric", "Date")) {
    
    return(NULL)
    
  }
  
  
  # take the final data points in the plot data
  data_arr <- data %>%
    group_by({{ breakdown }}) %>%
    filter(!is.na({{ y }})) %>%
    filter({{ x }} == max({{ x }}, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange({{ y }})
  
  if (nrow(data_arr) <= 1) {
    return(NULL)
  }
  
  y_axis <- data %>%
    pull({{ y }}) %>%
    range(na.rm = TRUE)
  
  y_axis_range <- diff(y_axis)
  
  x_axis <- data %>%
    pull({{ x }}) %>%
    range(na.rm = TRUE)
  
  x_axis_range <- diff(x_axis)
  
  plot_values <- data_arr %>%
    pull({{ y }})
  
  text_values <- data_arr %>%
    pull({{ y }})
  
  # Initial shift up -----------------------------------------------------------
  # Working up through the labels check if overlap with the previous one.
  # If so move up by a fixed amount
  near_zero <- abs(text_values[1]) < gap_factor*y_axis_range
  
  if (near_zero) {
    
    text_values[1] <- text_values[1] + gap_factor*y_axis_range
    
  }
  
  for (i in 2:nrow(data_arr)){
    
    diff <- text_values[i] - text_values[i - 1]
    overlap <- text_values[i] - text_values[i - 1] < gap_factor*y_axis_range
    
    if (overlap) {
      
      text_values[i] <- text_values[i] + gap_factor*y_axis_range - diff
      
    }
    
  }
  
  # Find runs of previously overlapping labels
  text_values
  
  init_text_values <- data_arr %>%
    pull({{ y }})
  
  moved <- init_text_values != text_values
  
  runs <- rle(moved)
  run_end <- cumsum(runs$lengths)
  run_start <- cumsum(runs$lengths) - runs$lengths + 1
  moved_start <- run_start[runs$values] - 1
  moved_end <- run_end[runs$values]
  
  for (i in seq_along(moved_start)) {
    
    if (moved_start[i] == 0) {
      moved_start[i] <- 1
    }
    
    start <- moved_start[i]
    end <- moved_end[i]
    
    value_mean <- mean(init_text_values[start:end])
    text_mean <- mean(text_values[start:end])
    
    mean_diff <- text_mean - value_mean
    label_diff <- text_values[start] - text_values[start - 1] - gap_factor*y_axis_range
    self_diff <- text_values - init_text_values
    
    text_values[end] <- text_values[end] - min(mean_diff, label_diff, self_diff[end], text_values[start] - gap_factor*y_axis_range)
    text_values[start:(end - 1)] <- text_values[start:(end - 1)] - min(mean_diff, label_diff, text_values[start] - gap_factor*y_axis_range)
    
  }
  
  text <- data_arr %>%
    pull({{ breakdown }})
  
  
  lines <- map2(
    .x = plot_values,
    .y = text_values,
    .f = function(plot_values, text_values, x_axis_range){
      list(
        type = "line",
        line = list(color = "black"),
        xref = "x",
        yref = "y",
        x0 = x_axis[2] + 0.005*x_axis_range,
        x1 = x_axis[2] + 0.05*x_axis_range,
        y0 = plot_values,
        y1 = text_values
      )
    },
    x_axis_range = x_axis_range
  )
  
  
  annotations <- map2(
    .x = text,
    .y = text_values,
    .f = function(text, text_values, x_axis, x_axis_range){
      list(
        text = as.character(text),
        x = x_axis[2] + 0.05*x_axis_range,
        y = text_values,
        xanchor = "left",
        yanchor = "middle",
        align = "right",
        valign = "top",
        showarrow = FALSE
      )
    },
    x_axis,
    x_axis_range
  )
  
  
  list(
    shapes = lines,
    annotations = annotations
  )
  
}