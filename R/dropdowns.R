dropdown_plots <- function(data, x, y, group_type, plot_type = c("bar", "line"), 
                           breakdown = NULL, orientation = "v",
                           x_axis_title = NULL, y_axis_title = NULL, plot_title = NULL,
                           gap_factor = 0.045, lci = NULL, uci = NULL, ...) {
  
  plot_type <- match.arg(plot_type)
  
  args <- list(...)
  
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
                  thickness = 1
    )
  }
  
  x_plot <- {{ x }}
  y_plot <- {{ y }}
  
  x <- formula_to_sym(x)
  y <- formula_to_sym(y)
  group_type <- formula_to_sym(group_type)
  
  if (is.null(breakdown)) {
    
    data_processed <- data %>%
      arrange(
        {{ group_type }}, {{ x }}
      ) %>%
      mutate(
        breakdown = {{ group_type }},
        label_text = {{ group_type }}
      )
    
    if (plot_type == "bar") {
      
      unique_breakdowns <- data_processed %>%
        distinct({{ group_type }}, {{ x }}) %>%
        group_by({{ x }}) %>%
        arrange({{ group_type }}) %>%
        mutate(n = row_number()) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(
          unique_breakdown = paste0(paste0(rep("<b></b>", n - 1), collapse = ""), {{ x }})
        ) %>%
        select(-n)
      
      data_processed <- data_processed %>%
        left_join(
          unique_breakdowns, by = intersect(colnames(.), colnames(unique_breakdowns))
        ) %>%
        mutate(
          {{ x }} := unique_breakdown
        ) %>%
        select(-unique_breakdown)
      
      data_processed <- data_processed %>%
        mutate(
          {{ x }} := factor({{ x }}, levels = unique({{ x }}))
        )
    }
      
    breakdown <- ~breakdown
    breakdown_plot <- {{ breakdown }}
    breakdown <- formula_to_sym(breakdown)
        
  } else {
    
    breakdown_plot <- {{ breakdown }}
    breakdown <- formula_to_sym(breakdown)
    
    unique_breakdowns <- data %>%
      distinct({{ group_type }}, {{ breakdown }}) %>%
      group_by({{ breakdown }}) %>%
      arrange({{ group_type }}) %>%
      mutate(n = row_number()) %>%
      rowwise() %>%
      mutate(
        unique_breakdown = paste0(paste0(rep("<b></b>", n - 1), collapse = ""), {{ breakdown }})
      ) %>%
      select(-n)
    
    
    data_processed <- data %>%
      left_join(
        unique_breakdowns, by = intersect(colnames(.), colnames(unique_breakdowns))
      ) %>%
      mutate(
        {{ breakdown }} := unique_breakdown
      ) %>%
      select(-unique_breakdown) %>%
      arrange(
        {{ group_type }}, {{ breakdown }}
      ) %>%
      mutate(
        label_text = {{breakdown }},
        {{ breakdown }} := factor({{ breakdown }}, levels = unique({{ breakdown }}))
      )
    
    if (is.character(pull(data, {{ x }})) | is.factor(pull(data, {{ x }}))) {
      
      data_processed <- data_processed %>%
        arrange({{ group_type }}, {{ x }}) %>%
        mutate(
          {{ x }} := factor({{ x }}, levels = unique({{ x }}))
        ) %>%
        arrange(
          {{ breakdown }}, {{ x }}
        )
      
    }
  }
  
  if (is.null(plot_title)) {
    
    plot_title <- ""
    
  }
  # take plot titles. Max 1 per group_type
  plot_titles <- data_processed %>%
    mutate(
      plot_title = glue({{ plot_title }})
    ) %>%
    distinct({{ group_type }}, plot_title) %>%
    pull(plot_title, name = {{ group_type }})
  
  plot_titles_anno <- map(
    .x = plot_titles,
    .f = function(.x) {
      list(
        text = .x,
        showarrow = FALSE,
        x = -0.05,
        y = 1,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom"
      )
    }
  )
  
  
  # split data into a list
  data_list <- data_processed %>%
    split(f = pull(., {{ group_type }}))
  
  # ordering data and finding number of breaks
  groups <- data_processed %>%
    distinct({{ group_type }}, {{ breakdown }}) %>%
    group_by({{ group_type }}, {{ breakdown }}) %>%
    summarise(
      n = n(),
      .groups = "drop"
    )
  
  # if we have bars AND lines then there will be an extra trace for each dropdown
  # option. this has to be added to the visible_list
  if (!is.null(args$bar_line)) {
    
    groups_line <- data_processed %>%
      distinct({{ group_type }})
    
    groups <- groups %>%
      bind_rows(groups_line) %>%
      arrange({{ group_type }}, {{ breakdown }})
    
  }
  
  visible_list <- map(
    .x = unique(pull(groups, {{ group_type }})),
    .f = function(.x, groups) {
      pull(groups, {{ group_type }}) == .x
    },
    groups = groups
  )
  
  
  
  n_colours <- data_processed %>%
    distinct({{ group_type }}, {{ breakdown }}) %>%
    group_by({{ group_type }}) %>%
    summarise(
      n = n(),
      .groups = "drop"
    )
  
  colour_palette <- map(
    .x = pull(n_colours, n),
    .f = function(n){
      make_colour_palette(n, "PuBu")
    }
  ) %>%
    flatten_chr()
  
  
  plot_init <- plot_ly()
  
  visible <- rep(FALSE, length(visible_list))
  visible[1] <- TRUE
  
  
  plot <- switch(
    plot_type,
    "bar" = reduce2(
      .x = data_list,
      .y = visible,
      .f = function(plot, data, visible, x_plot, y_plot, breakdown_plot, errors, ...){
        bar_chart(
          plot = plot,
          data = data,
          x = x_plot,
          visible = visible,
          y = y_plot,
          breakdown = breakdown_plot,
          colour_palette = colour_palette,
          error_y = errors,
          ...
        )
      },
      x_plot = x_plot,
      y_plot = y_plot,
      breakdown_plot = breakdown_plot,
      errors = errors,
      ...,
      .init = plot_init
    ),
    "line" = reduce2(
      .x = data_list,
      .y = visible,
      .f = function(plot, data, visible, x_plot, y_plot, breakdown_plot, gap_factor, ...){
        line_chart(
          plot = plot,
          data = data,
          x = x_plot,
          visible  = visible,
          y = y_plot,
          breakdown = breakdown_plot,
          add_labels = visible,
          gap_factor = gap_factor,
          colour_palette = colour_palette,
          ...
        )
      },
      x_plot = x_plot,
      y_plot = y_plot,
      breakdown_plot = breakdown_plot,
      gap_factor = gap_factor,
      ...,
      .init = plot_init
    )
  )
  
  # Add labels -----------------------------------------------------------------
  if (plot_type == "line"){
    annotations <- map(
      .x = data_list,
      .f = function(data, x_plot, y_plot, label_text, gap_factor){
        add_labels(
          data = data,
          x = x_plot,
          y = y_plot,
          breakdown = ~label_text,
          gap_factor = gap_factor
        )
      },
      x_plot = x_plot,
      y_plot = y_plot,
      gap_factor = gap_factor
    )
    
    buttons <- pmap(
      list(
        visible_list = visible_list,
        annotation = annotations,
        name = names(data_list)
      ),
      .f = function(visible_list, annotation, name) {
        list(
          method = "update",
          args = list(
            list(visible = visible_list),
            list(
              annotations = annotation$annotations,
              shapes =  annotation$shapes
            )
          ),
          label = name
        )
      }
    )
    
  } else {
    
    buttons <- map2(
      visible_list,
      names(data_list),
      .f = function(visible_list, names){
        
        list(
          method = "restyle",
          args = list(
            "visible", visible_list
          ),
          label = names
        )
        
      }
    )
    
  }
  
  #
  
  
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
    itemclick = "toggleothers"
  )
  
  plot %>%
    plotly::layout(
      updatemenus = list(
        list(
          showactive = TRUE,
          active = 0,
          x = -0.05,
          y = 1.3,
          xanchor = "left",
          buttons = buttons
        )
      ),
      font = global_font,
      legend = legend_options,
      autosize = TRUE,
      xaxis = list(title = x_axis_title),
      yaxis = list(
        title = y_axis_title,
        titlefont = list(
          color = "#FFFFFFFF",
          size = 0
        ),
        rangemode = "tozero"
      ),
      plot_bgcolor = "transparent"
    ) %>%
    add_annotations(
      text = y_axis_title,
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



# plot <- line_chart(
#   plot = plot_init,
#   data = data_processed[[1]],
#   x = x_plot,
#   visible  = TRUE,
#   y = y_plot,
#   breakdown = group_plot,
#   add_labels = FALSE,
#   showlegend = FALSE,
#   colour_palette = colour_palette,
#   ...
# ) %>%
#   line_chart(
#     data = data_processed[[2]],
#     x = x_plot,
#     visible  = TRUE,
#     y = y_plot,
#     breakdown = group_plot,
#     add_labels = FALSE,
#     showlegend = FALSE,
#     colour_palette = colour_palette,
#     ...
#   )
# 
# plot %>%
#   layout(
#     updatemenus = list(
#       list(
#         active = 0,
#         x = 0,
#         y = 1.2,
#         xanchor = "left",
#         buttons = list(
#           list(
#             method = "restyle",
#             args = list(
#               "visible", c(TRUE, TRUE, FALSE, FALSE)
#             ),
#             label = "A"
#           ),
#           list(
#             method = "restyle",
#             args = list(
#               "visible", c(FALSE, FALSE, TRUE, TRUE)
#             ),
#             label = "B"
#           )
#         )
#       )
#     )
#   )
