dropdown_plots <- function(data, x, y, dropdown, plot_type = c("bar", "line"), 
                           breakdown = NULL, orientation = "v", 
                           palette = "PuBu",
                           text = NULL, xnudge = 0, ynudge = 0,
                           x_axis_title = NULL, y_axis_title = NULL, plot_title = "",
                           gap_factor = 0.045, lci = NULL, uci = NULL,
                           x_axis_options = NULL, y_axis_options = NULL,
                           fix_axes = TRUE, bar_line = NULL, figure_name = NULL, ...) {
  
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
    save_data(data, x = x, y = y, breakdown = breakdown, bar_line = bar_line, dropdown = dropdown, figure_name = figure_name)
  }
  
  x_plot <- {{ x }}
  y_plot <- {{ y }}
  bar_line_plot <- {{ bar_line }}
  
  x <- formula_to_sym(x)
  y <- formula_to_sym(y)
  dropdown <- formula_to_sym(dropdown)
  bar_line <- formula_to_sym(bar_line)
  
  # Fixing factors -------------------------------------------------------------
  # This is a bit of mess - sorry - I imagine it can be cleaned up
  # The idea is that we need the breakdowns for every plot to be unique between 
  # plots. Sometimes we'd actually like the same breakdowns in each plot, so we 
  # add <b></b> to the start of breakdowns to make them unique. A bit nasty, but 
  # an easy way to make them unique to R but display exactly the same.
  # Similarly, if we order our data by the x column (for bar plots) it should 
  # never mix up x values between traces. e.g. it shouldn't go Male, 0-17, Female,
  # 18-24, say. It should go Male, Female, 0-17, 18-24. Otherwise when selecting 
  # between dropdowns you'll see categories from other traces.
  if (is.null(breakdown)) {
    
    data_processed <- data %>%
      arrange(
        {{ dropdown }}, {{ x }}
      ) %>%
      mutate(
        breakdown = {{ dropdown }},
        label_text = {{ dropdown }}
      )
    
    if (plot_type == "bar") {
      
      unique_breakdowns <- data_processed %>%
        distinct({{ dropdown }}, {{ x }}) %>%
        group_by({{ x }}) %>%
        arrange({{ dropdown }}) %>%
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
    
    data_processed <- data %>%
      make_unique_values({{ dropdown }}, {{ breakdown }}, "breakdown_orig") %>%
      mutate(
        label_text = {{ breakdown }}
      )

    
    if (is.character(pull(data, {{ x }})) | is.factor(pull(data, {{ x }}))) {
      
      data_processed <- data_processed  %>%
        make_unique_values({{ dropdown }}, {{ x }}, "x_orig") %>%
        arrange(
          {{ x }}, {{ breakdown }}
        )
      
    }
  }
  
  # take plot titles. Max 1 per dropdown
  plot_titles <- data_processed %>%
    distinct({{ dropdown }}) %>%
    mutate(
      plot_name = {{ plot_title }}
    ) %>%
    rowwise() %>%
    mutate(
      plot_name = glue(plot_name)
    ) %>%
    pull(plot_name, name = {{ dropdown }})
  
  plot_titles <- paste0("<b>", plot_titles, "</b>", "\n\n", y_axis_title)
  
  plot_titles_anno <- map(
    .x = plot_titles,
    .f = function(.x) {
      list(
        text = .x,
        align = "left",
        showarrow = FALSE,
        x = -0.05,
        y = 1,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        label = "title"
      )
    }
  )
  
  # split data into a list
  data_list <- data_processed %>%
    split(f = pull(., {{ dropdown }}))
  
  # ordering data and finding number of breaks
  groups <- data_processed %>%
    distinct({{ dropdown }}, {{ breakdown }}) %>%
    group_by({{ dropdown }}, {{ breakdown }}) %>%
    summarise(
      n = n(),
      .groups = "drop"
    )
  
  # if we have bars AND lines then there will be an extra trace for each dropdown
  # option. this has to be added to the visible_list
  if (!is.null(bar_line)) {
    
    line_breakdown <- formula_to_sym(args$line_breakdown)
    
    groups_line <- data_processed %>%
      group_by({{ dropdown }}, {{ line_breakdown }}) %>%
      summarise(
        all_nas = all(is.na({{ bar_line }})),
        .groups = "drop"
      ) %>%
      filter(!all_nas)
    
    groups <- groups %>%
      bind_rows(groups_line) %>%
      arrange({{ dropdown }}, {{ breakdown }})
    
  }
  
  visible_list <- map(
    .x = unique(pull(groups, {{ dropdown }})),
    .f = function(.x, groups) {
      pull(groups, {{ dropdown }}) == .x
    },
    groups = groups
  )
  
  # if using the same colour scheme between plots match up colours
  # if not then make a colour palette for each dropdown
  if (length(palette) > 1) {
    
    data_processed$palette <- palette[data_processed$breakdown_orig]
    
    palette_df <- distinct(data_processed, {{ breakdown }}, palette)
    
    palette_names <- pull(palette_df, {{ breakdown }})
    
    colour_palette <- pull(palette_df, palette) %>%
      set_names(palette_names)
    
  } else {
    
    n_colours <- data_processed %>%
      distinct({{ dropdown }}, {{ breakdown }}) %>%
      group_by({{ dropdown }}) %>%
      summarise(
        n = n(),
        .groups = "drop"
      )
    
    colour_palette <- map(
      .x = pull(n_colours, n),
      .f = function(n){
        make_colour_palette(n, palette)
      }
    ) %>%
      flatten_chr()
  
  }
  
  plot_init <- plot_ly()
  
  visible <- rep(FALSE, length(visible_list))
  visible[1] <- TRUE
  
  
  # Run through plots ----------------------------------------------------------
  # Run through data_list turning each one into a plot trace
  # Only the first plot will be visible at first
  plot <- switch(
    plot_type,
    # Bar plot -----------------------------------------------------------------
    "bar" = reduce2(
      .x = data_list,
      .y = visible,
      .f = function(plot, data, visible, x_plot, y_plot, breakdown_plot, errors, orientation, ...){
        plot <- bar_chart(
          plot = plot,
          data = arrange_(data, {{ breakdown_plot }}),
          x = x_plot,
          visible = visible,
          y = y_plot,
          bar_line = bar_line_plot,
          breakdown = breakdown_plot,
          colour_palette = colour_palette,
          error_y = error_y,
          error_x = error_x,
          orientation = orientation,
          ...
        ) %>% 
          layout(
            yaxis = list(
              titlefont = list(
                color = "#FFFFFFFF",
                size = 0
              ),
              rangemode = "tozero"
            )
          )
        
        plot
        
        
      },
      x_plot = x_plot,
      y_plot = y_plot,
      breakdown_plot = breakdown_plot,
      errors = errors,
      orientation = orientation,
      ...,
      .init = plot_init
    ),
    # Line plot ----------------------------------------------------------------
    "line" = reduce2(
      .x = data_list,
      .y = visible,
      .f = function(plot, data, visible, x_plot, y_plot, breakdown_plot, gap_factor, ...){
        plot <- line_chart(
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
        ) %>% 
          layout(
            yaxis = list(
              titlefont = list(
                color = "#FFFFFFFF",
                size = 0
              ),
              rangemode = "tozero"
            )
          )
        
        plot
        
      },
      x_plot = x_plot,
      y_plot = y_plot,
      breakdown_plot = breakdown_plot,
      gap_factor = gap_factor,
      ...,
      .init = plot_init
    )
  )
  
  # Extra annotations ----------------------------------------------------------
  extra_annotation_list <- map(
    data_list,
    function(data) {
      make_extra_labels(data, text, x_plot, y_plot, xnudge, ynudge)
    }
  )
  
  # returns null for bar plot
  annotations <- dropdowns_add_labels(
    data_list, plot_type, x_plot, y_plot, label_text, gap_factor, extra_annotation_list
  )
  
  # Create buttons for dropdown ------------------------------------------------
  buttons <- dropdowns_add_buttons(
    data_list, annotations, visible_list, plot_titles_anno
  )
  
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
  
  # Final plot with formatting -------------------------------------------------
  # Make plot with buttons and other formatting
  plot <- plot %>%
    plotly::layout(
      updatemenus = list(
        list(
          showactive = TRUE,
          x = -0.05,
          y = 1.3,
          xanchor = "left",
          buttons = buttons
        )
      ),
      font = global_font,
      legend = legend_options,
      autosize = TRUE,
      plot_bgcolor = "transparent",
      yaxis = y_axis_options,
      xaxis = x_axis_options
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
  
  if (fix_axes) {
    plot <- axis_buttons_fix(plot, y_axis_title, x_axis_title)
  }

  plot %>%
  layout(
    annotations = append(plot_titles_anno[1], extra_annotation_list[[1]]),
    xaxis = list(
      title = x_axis_title[1]
    ),
    yaxis = list(
      title = y_axis_title[1]
    )
  )
  
}


