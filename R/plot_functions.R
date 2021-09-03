#' Title
#'
#' @param data 
#' @param x 
#' @param y 
#' @param breakdown 
#' @param legend_group 
#' @param lci 
#' @param uci 
#' @param x_axis_title 
#' @param y_axis_title 
#' @param plot_title 
#' @param palette 
#' @param hover_over 
#' @param plot_type 
#' @param line_mode 
#' @param line_connect_gaps 
#' @param barmode 
#' @param orientation 
#' @param bar_line 
#' @param bar_line_name 
#' @param legend_name 
#' @param showlegend 
#' @param add_labels 
#' @param gap_factor 
#' @param transforms 
#' @param dtick 
#' @param tick0 
#' @param y_axis_range 
#' @param line_hover_over 
#' @param top_margin 
#'
#' @return
#' @export
#'
#' @examples
make_plot <- function(data, x, y, breakdown = NULL, legend_group = breakdown, lci = NULL, uci = NULL, 
                      x_axis_title = NULL, y_axis_title = NULL, plot_title = NULL,
                      palette = "PuBu", hover_over = NULL, plot_type = c("bar", "line"),
                      line_mode = "lines", line_connect_gaps = TRUE,
                      barmode = "group", orientation = "v",  line_colour = "#FF7F0E",
                      bar_line = NULL, bar_line_name = NULL, legend_name = NULL, showlegend = TRUE,
                      add_labels = TRUE, gap_factor = 0.045, transforms = NULL,
                      dtick = NULL, tick0 = NULL, text = NULL, xnudge = 0, ynudge = 0,
                      y_axis_range = NULL, line_hover_over = hover_over, top_margin = 70,
                      figure_name = NULL){
  
  # Set up ---------------------------------------------------------------------
  plot_type <- match.arg(plot_type)
  
  # error bars
  if (is.null(lci) | is.null(uci)) {
    errors <- NULL
  } else {
    
    lci_sym <- formula_to_sym({{ lci }})
    uci_sym <- formula_to_sym({{ uci }})
    y_sym <- formula_to_sym({{ y }})
    
    data <- data %>%
      mutate(
        {{ lci_sym }} := {{ y_sym }} - {{ lci_sym }},
        {{ uci_sym }} := {{ uci_sym }} - {{ y_sym }}
      )
    
    errors = list(visible = TRUE,
                  symmetric = FALSE,
                  type = 'data',
                  array = {{ uci }},
                  arrayminus = {{ lci }},
                  color = "#000000",
                  width = 5,
                  thickness = 2
    )
  }
  
  # swapping labels for horizontal bar plot
  if (orientation == "h") {
    x_axis_temp <- x_axis_title
    y_axis_temp <- y_axis_title
    
    x_axis_title <- y_axis_temp
    y_axis_title <- x_axis_temp
    
    error_x <- errors
    error_y <- NULL
    
    y_temp <- y
    y <- x
    x <- y_temp
    
  } else {
    
    error_x <- NULL
    error_y <- errors
    
  }
  
  # Save data ------------------------------------------------------------------
  if (!is.null(figure_name)) {
    save_data(data, x = x, y = y, breakdown = breakdown, bar_line = bar_line, figure_name = figure_name)
  }
  
  # ordering data and finding number of breaks
  if (!is.null(breakdown)){
    # breakdown is 
    n <- data[[as.character(breakdown)[2]]] %>%
      unique() %>%
      length()
    
    if (length(palette) == 1) {
      colour_palette <- make_colour_palette(n, palette)
    } else {
      colour_palette <- palette
    }
    
    # plotly occasionally bugs out when there is missing data. arranging by the 
    # breakdown fixes this. we use arrange_() instead of arrange() because we 
    # pass columns names as formulae (with a ~ at the start) for plotly
    data <- data %>%
      # arranging by x first can fix some weird issues with line plots where 
      # they make loops. NOTE: this isn't the same as doing arrange(x, breakdown)!!
      arrange_({{ x }}) %>%
      arrange_({{ breakdown }})
    
    
  } else {
    
    if (is.null(legend_name)) {
      
      legend_name <- ""
      
    }
    
    data <- data %>%
      mutate(
        breakdown = legend_name
      )
    
    breakdown <- ~breakdown
    
    # plotly occasionally bugs out when there is missing data. arranging by the 
    # breakdown fixes this. we use arrange_() instead of arrange() because we 
    # pass columns names as formulae (with a ~ at the start) for plotly
    data <- data %>%
      arrange_({{ x }}) %>%
      arrange_({{ breakdown }})
    
    colour_palette <- "#2B8CBE"
  }
  
  
  # Legend formatting
  legend_options <- list(
    x = 0,
    y = -0.5,
    orientation = 'h',
    xanchor = "left",
    yanchor = "bottom",
    xref = "paper",
    yref = "paper",
    bgcolor = "transparent",
    itemclick = "toggleothers",
    traceorder = "normal"
  )
  
  # Plot data ------------------------------------------------------------------
  # either line or bar plot
  
  plot <- plot_ly()
  
  plot <- switch(
    plot_type,
    "bar" = bar_chart(
      plot = plot, data = data, x = x, y = y, breakdown = breakdown,
      hover_over = hover_over, orientation = orientation,
      bar_line = bar_line, bar_line_name = bar_line_name, line_colour = line_colour,
      legend_name = legend_name, colour_palette = colour_palette, 
      barmode = barmode, error_x = error_x, error_y = error_y,
      showlegend = showlegend, legend_group = legend_group, transforms = transforms,
      line_hover_over = line_hover_over
    ),
    "line" = line_chart(
      plot = plot, data = data, x = x, y = y, breakdown = breakdown, colour_palette = colour_palette, 
      hover_over = hover_over, line_connect_gaps = line_connect_gaps, error_y = error_y,
      line_mode = line_mode, add_labels = add_labels, gap_factor = gap_factor,
      showlegend = showlegend, legend_group = legend_group,
      transforms = transforms, legend_name = legend_name
    )
  )
  
  tick0_dtick <- tick0_dtick(data, {{ x }}, tick0, dtick)
  
  # Extra annotations ----------------------------------------------------------
  extra_annotation_list <- make_extra_labels(data, text, x, y, xnudge, ynudge)
  
  # extra layout options
  plot %>% 
    plotly::layout(
      font = global_font,
      legend = legend_options,
      autosize = TRUE,
      xaxis = list(
        title = x_axis_title,
        tick0 = tick0_dtick$tick0,
        dtick = tick0_dtick$dtick
      ),
      # hide the y-axis label from its usual place - we want it on the top of
      # the y axis, horizontally
      yaxis = list(
        title = y_axis_title,
        titlefont = list(
          color = "#FFFFFFFF",
          size = 1
        ),
        rangemode = "tozero"
      ),
      plot_bgcolor = "transparent",
      margin = list(t = top_margin),
      annotations = extra_annotation_list
    ) %>%
    # add the y-axis label as an annotation
    add_annotations(
      text = paste0("<b>", plot_title, "</b>", "\n\n", y_axis_title),
      align = "left",
      showarrow = FALSE,
      x = -0.05,
      y = 1,
      xref = "paper",
      yref = "paper",
      xanchor = "left",
      yanchor = "bottom"
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtons = list(
        list("toImage", "zoomIn2d", 
             "zoomOut2d", "pan2d",
             "zoom2d", "resetScale2d"
        )
      ),
      toImageButtonOptions = list(
        format = "png",
        filename = "health_profiles_for_england_plot.png",
        width = 900,
        height = 640
      )
    )
  
}

# Line chart -------------------------------------------------------------------
line_chart <- function(plot, data, x, y, colour_palette, breakdown = NULL, legend_group = breakdown,
                       hover_over = NULL, showlegend = TRUE,
                       line_connect_gaps = FALSE, error_y = NULL,
                       line_mode = "lines", add_labels = TRUE, gap_factor = 0.045,
                       transforms = NULL, visible = TRUE, legend_name = NULL){
  
  if (line_mode == "lines") {
    
    line <- list(width = 3)
    
  } else {
    
    line <- NULL
    
  }
  
  plot <- plot %>%
    add_trace(
      data = data,
      x = {{ x }},
      y = {{ y }},
      hovertemplate = hover_over,
      type = "scatter",
      mode = line_mode,
      name = legend_name,
      legendgroup = {{ breakdown }},
      color = {{ breakdown }},
      colors = colour_palette,
      line = line,
      connectgaps = line_connect_gaps,
      error_y = error_y,
      transforms = transforms,
      visible = visible,
      showlegend = showlegend
    )
  
  if (add_labels & !is.null(breakdown)) {
    
    labels <- data %>%
      add_labels(
        x, 
        y,
        breakdown,
        gap_factor
      )
    
    plot <- plot %>%
      plotly::layout(
        shapes = labels$shapes,
        annotations = labels$annotations,
        showlegend = FALSE
      )
    
  }
  
  plot
  
}

# Bar Chart --------------------------------------------------------------------
bar_chart <- function(plot, data, x, y, breakdown = NULL, legend_group = breakdown,
                      hover_over = NULL, orientation = "v", showlegend = TRUE,
                      bar_line = NULL, bar_line_name = NULL, line_colour = "#FF7F0E",
                      legend_name = NULL, colour_palette, 
                      barmode = "group", error_x = NULL, error_y = NULL, transforms = NULL,
                      visible = TRUE, line_hover_over = hover_over, line_breakdown = NULL) {
  
  plot <- plot %>%
    add_trace(
      data = data,
      x = {{ x }},
      y = {{ y }},
      hovertemplate = hover_over,
      type = 'bar',
      showlegend = showlegend,
      name = legend_name,
      split = {{ breakdown }},
      color = {{ breakdown }},
      legendgroup = {{ legend_group }},
      colors = colour_palette,
      orientation = orientation,
      error_y = error_y,
      error_x = error_x,
      visible = visible,
      transforms = transforms
    ) %>% 
    plotly::layout(
      barmode = barmode
    )
  
  # add line if specified
  if (!is.null(bar_line)) {
    
    bar_line_sym <- formula_to_sym(bar_line)
    
    all_nas <- pull(data, {{ bar_line_sym }}) %>%
      is.na() %>%
      all()
    
    if (!all_nas) {
    
      plot <- plot %>%
        add_trace(
          x = {{ x }},
          y = {{ bar_line }},
          legendgroup = "line",
          type = 'scatter',
          mode = 'lines', 
          split = {{ line_breakdown }},
          linetype = {{ line_breakdown }},
          name = bar_line_name,
          inherit = FALSE,
          line = list(color = line_colour),
          hovertemplate = line_hover_over,
          showlegend = showlegend,
          visible = visible
        )
    }
  }
  
  plot
}
