# Subplots ---------------------------------------------------------------------
make_subplots_dropdowns <- function(
  data, x, y, subplot, dropdown, breakdown = NULL, nrows = 2, margin = 0.07, shareY = TRUE, 
  shareX = TRUE, titleX = shareX, titleY = shareY, bar_line = NULL, complete_data = FALSE,
  showlegend = TRUE, plot_title = "", figure_name = NULL, y_axis_title = NULL, 
  x_axis_title = NULL, orientation = "v", ...) {
  
  # Save data ------------------------------------------------------------------
  if (!is.null(figure_name)) {
    save_data(
      data, x = x, y = y, 
      breakdown = breakdown, bar_line = bar_line, 
      subplot = subplot, dropdown = dropdown, 
      figure_name = figure_name
    )
  }
  
  subplot_plot <- {{ subplot }}
  subplot <- formula_to_sym(subplot)
  
  dropdown_plot <- {{ dropdown }}
  dropdown <- formula_to_sym(dropdown)
  
  breakdown_plot <- {{ breakdown }}
  breakdown <- formula_to_sym(breakdown)
  
  x_plot <- {{ x }}
  x <- formula_to_sym(x)
  
  y_plot <- {{ y }}
  y <- formula_to_sym(y)
  
  bar_line_plot <- {{ bar_line }}
  bar_line <- formula_to_sym(bar_line)
  
  # if you have a different number of breakdowns in each subplot things will 
  # display weirdly - completing the data can help
  if (complete_data) {
    
    data <- complete_data(data, x, y, dropdown, subplot, breakdown, bar_line)
    
  }
  
  # Add plot title to first plot in every dropdown option, but no other plots
  plot_title_df <- data %>%
    distinct(
      {{ dropdown }},
      {{ subplot }}
    ) %>%
    arrange({{ dropdown }}, {{ subplot }}) %>%
    group_by(
      {{ dropdown }}
    ) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      plot_title = {{ plot_title }}
    )
  
  
  plots <- data %>%
    mutate(
      showlegend = if_else({{ subplot }} == {{ subplot }}[1], showlegend, FALSE),
      group = paste0("<b>", {{ subplot }}, "</b>")
    ) %>%
    # add on plot title data
    left_join(
      plot_title_df, by = intersect(colnames(.), colnames(plot_title_df))
    ) %>%
    mutate(
      plot_title = if_else(is.na(plot_title), "", plot_title)) %>%
    # make nested data frame and plot
    group_nest({{ subplot }}, keep = TRUE) %>%
    mutate(
      plots = map(
        .x = data,
        .f = function(.x, nrows, dropdown_plot, y_axis_title, x_axis_title, orientation, ...){
          
          if (orientation == "h") {
            x_axis_title <- paste0("<b>", unique(.x$group), "</b>", "\n", x_axis_title)
          } else {
            y_axis_title <- paste0("<b>", unique(.x$group), "</b>", "\n", y_axis_title)
          }
          
          plot <- dropdown_plots(
            data = .x,
            dropdown = dropdown_plot,
            fix_axes = FALSE,
            plot_title = unique(.x$plot_title),
            x = x_plot,
            y = y_plot,
            y_axis_title = y_axis_title,
            x_axis_title = x_axis_title,
            orientation = orientation,
            bar_line = bar_line_plot,
            breakdown = breakdown_plot,
            ...,
            showlegend = unique(.x$showlegend)
          )
          
          plot
        },
        nrows = nrows,
        dropdown_plot = dropdown_plot,
        y_axis_title = y_axis_title,
        x_axis_title = x_axis_title,
        orientation = orientation,
        ...
      )
    )
  
  # subplot can take our fancy, nested data-frame as an argument
  subplots <- plots %>%
    subplot(
      nrows = nrows,
      shareX = shareX,
      shareY = shareY,
      titleY = titleX,
      titleX = titleY,
      margin = margin
    ) %>%
    layout(
      margin = list(
        t = 60,
        b = 100
      )
    )
  
  
  # this will fix the axes once a button is pressed, but still leaves us without
  # when the plot initialises
  subplots <- extra_label_fix(subplots, plots, subplot, nrows, shareX, shareY)
  subplots <- annotations_fix(subplots, plots)
  
  # need to swap the x and y axis for this
  # couldn't do it inearlier in this script as they'd end up swapped back in the 
  # dropdowns function!
  if (orientation == "h") {
    x_axis_temp <- x_axis_title
    y_axis_temp <- y_axis_title
    
    x_axis_title <- y_axis_temp
    y_axis_title <- x_axis_temp
    
  }
  
  subplots <- axis_buttons_fix(subplots, y_axis_title, x_axis_title)

  # add back in the x and y axis titles for the initial plot
  x_axes <- names(subplots$x$layout[grepl("^xaxis", names(subplots$x$layout))])
  y_axes <- names(subplots$x$layout[grepl("^yaxis", names(subplots$x$layout))])

  x_axes_title <- map(
    set_names(x_axes, x_axes),
    ~list(title = x_axis_title[1])
  )

  y_axes_title <- map(
    set_names(y_axes, y_axes),
    ~list(title = y_axis_title[1])
  )

  axes <- append(x_axes_title, y_axes_title)
  p <- list(p = subplots)
  subplots <- do.call(layout, append(p, axes))
  
  subplots

}
