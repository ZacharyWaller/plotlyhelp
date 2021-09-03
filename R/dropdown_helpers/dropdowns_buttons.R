# Add labels to dropdown plot --------------------------------------------------
dropdowns_add_labels <- function(data_list, plot_type, x_plot, y_plot, label_text, gap_factor, source_list) {
  
  if (plot_type == "line"){
    # here the annotations are lines and shapes for line plots
    annotations <- map2(
      .x = data_list,
      .y = source_list,
      .f = function(data, source_list, x_plot, y_plot, label_text, gap_factor){
        labels <- add_labels(
          data = data,
          x = x_plot,
          y = y_plot,
          breakdown = ~label_text,
          gap_factor = gap_factor
        )
        
        labels$annotations <- prepend(labels$annotations, source_list)
        
        labels
        
      },
      x_plot = x_plot,
      y_plot = y_plot,
      gap_factor = gap_factor
    )
    
  } else {
    
    annotations <- vector(length = length(data_list), mode = "list")
    annotations <- map2(
      annotations,
      source_list,
      function(element, source_list) {
        element$annotations = source_list
        element
      }
    )
    
  }
  
  annotations
  
}

# Add buttons to dropdown plots ------------------------------------------------
dropdowns_add_buttons <- function(data_list, annotations, visible_list, plot_title) {
  
  # apply the function to make a list of buttons
  pmap(
    list(
      visible_list = visible_list,
      annotation = annotations,
      name = names(data_list),
      plot_title = plot_title
    ),
    make_buttons
  )
  
}

# function for making the buttons
make_buttons <- function(visible_list, annotation, name, plot_title) {
  
  # final list - note that the elements of args aren't names and that the
  # "visible" bit is in its own list. not entirely clear about why that is, 
  # but things will break if not in this format
  list(
    method = "update",
    label = name,
    args = list(
      list(visible = visible_list),
      list(
        annotations = append(annotation$annotations, list(plot_title)),
        shapes = annotation$shapes
      )
    )
  )
}


axis_buttons_fix <- function(plot, y_axis_title, x_axis_title) {
  # converting the plot to a subplot allows us to extract some of the x and y axis
  # data
  if (is.null(plot$x$subplot)) {
    plot <- subplot(plot)
  }
  
  # find the x and y axes 
  # for subplots there may be multiple of each
  plot_layout <- plot$x$layout
  
  y_axis_list <- plot_layout[grepl("^yaxis", names(plot_layout))]
  
  x_axis_list <- plot_layout[grepl("^xaxis", names(plot_layout))]
  
  # so long as there is only one button this will work, otherwise need to 
  # loop over elements in updatemenus
  plot_buttons <- plot$x$layout$updatemenus[[1]]$buttons
  
  new_args <- pmap(
    list(
      option = plot_buttons,
      y_axis_title = y_axis_title,
      x_axis_title = x_axis_title,
      y_axis_list = list(y_axis_list),
      x_axis_list = list(x_axis_list)
    ),
    make_new_args
  )
  
  for (i in seq_along(new_args)) {
    
    plot$x$layout$updatemenus[[1]]$buttons[[i]]$args[[2]] <- new_args[[i]]
    
  }
  
  plot
  
}

# Add x and y axis titles to annotations
make_new_args <- function(option, y_axis_title, x_axis_title, y_axis_list, x_axis_list, annotation_list) {
  
  y_axes <- map(
    y_axis_list,
    function(.x) {
      .x$title <- y_axis_title
      .x
    }
  )
  
  x_axes <- map(
    x_axis_list,
    function(.x) {
      .x$title <- x_axis_title
      .x
    }
  )
  
  c(option$args[[2]], y_axes, x_axes)
  
}


# Annotations fix --------------------------------------------------------------
annotations_fix <- function(subplots, plots) {
  
  buttons <- subplots$x$layout$updatemenus[[1]]$buttons
  
  for (plot_no in seq_along(plots$plots)) {
    
    plot_as_subplot <- subplot(plots$plots[[plot_no]])
    
    for (button_no in seq_along(buttons)) {
    
      # take the text from the buttons
      button_annotation <- last(plot_as_subplot$x$layout$updatemenus[[1]]$buttons[[button_no]]$args[[2]]$annotations)
      text_from_button <- button_annotation$text
      
      # take everything else from the initial annotations in the subplot
      title_annotations <- map(
        subplots$x$layout$annotations,
        ~.x$label == "title"
      ) %>%
        as.logical()
      
      layout_from_intitial <- subplots$x$layout$annotations[title_annotations][[plot_no]]
      layout_from_intitial$text <- text_from_button
      
      if (plot_no == 1) {
        all_indices <- seq_along(subplots$x$layout$updatemenus[[1]]$buttons[[button_no]]$args[[2]]$annotations)
        
        index <- last(all_indices)
        
        subplots$x$layout$updatemenus[[1]]$buttons[[button_no]]$args[[2]]$annotations[[index]] <- layout_from_intitial
      } else {
        subplots$x$layout$updatemenus[[1]]$buttons[[button_no]]$args[[2]]$annotations <- append(
          subplots$x$layout$updatemenus[[1]]$buttons[[button_no]]$args[[2]]$annotations,
          list(layout_from_intitial)
        )
      }
    
    }
  }
  
  subplots
  
}


# Extra labels fix -------------------------------------------------------------
# If we've added extra labels we lose them when we combine the plots into subplots
# Getting them back in is a pain in the ass, as you're about to see.
# Wee need to take the annotations from the orignial plots and add them to the 
# buttons for the combined plots (combining plots into subplots only takes the 
# buttons from the first plot. I can't fnid a way to avoid this - so I had to do 
# all the madness below. 
# You're in for some seriously heavy shit.
extra_label_fix <- function(subplots, plots, subplot, nrows, shareX, shareY) {
  
  plots
  
  plot_names <- pull(plots, !! subplot)
  
  ncols <- ceiling(length(plot_names)/nrows)
  
  if (shareY) {
    n_y <- nrows
  } else {
    n_y <- length(plot_names)
  }
  
  if (shareX) {
    n_x <- ncols
  } else {
    n_x <- length(plot_names)
  }
  
  x_axes <- sort(names(subplots$x$layout[grepl("^xaxis", names(subplots$x$layout))]))
  x_ref <- gsub("axis", "", x_axes)
  y_axes <- sort(names(subplots$x$layout[grepl("^yaxis", names(subplots$x$layout))]))
  y_ref <- gsub("axis", "", y_axes)
  
  for (i in seq_along(plot_names)) {
    
    # first plot's annotations are already dealt with by the subplot function
    if (i == 1) {
      next
    }
    
    x_pos <- ((i - 1) %% n_x) + 1
    y_pos <- ((i - 1) %% n_y) + 1
    
    # find current plot's x and y ref names
    x_ref_curr <- x_ref[x_pos]
    y_ref_curr <- y_ref[y_pos]
    
    plot_as_subplot <- subplot(plots$plots[[i]])
    
    buttons <- plot_as_subplot$x$layout$updatemenus[[1]]$buttons
    
    new_button_annotations <- list()
    
    # Across buttons -----------------------------------------------------------
    for (button_i in seq_along(buttons)) {
    
      button <- buttons[[button_i]]
      
      button_annotations <- button$args[[2]]$annotations
      
      is_extra_annotations <- map(
        button_annotations,
        ~.x$label == "extras"
      ) %>%
        as.logical()
      
      extra_annotations <- button_annotations[is_extra_annotations]
      
      new_annotations <- list()
      
      # Across annotations -----------------------------------------------------
      for (annotation in extra_annotations) {
        
        annotation$xref <- x_ref_curr
        annotation$yref <- y_ref_curr
        
        new_annotations <- append(new_annotations, list(annotation))
        
      }
      
      subplots$x$layout$updatemenus[[1]]$buttons[[button_i]]$args[[2]]$annotations <- append(
        new_annotations,
        subplots$x$layout$updatemenus[[1]]$buttons[[button_i]]$args[[2]]$annotations
      )
        
      
    }
    
  }
  
  subplots
  
  
}

# 
source_fix <- function(subplots) {
  
  annotations <- subplots$x$layout$updatemenus[[1]]$buttons[[1]]$args[[2]]$annotations
  
  source_annotations <- map(
    annotations,
    ~.x$label == "extras"
  ) %>%
    as.logical()
  
  source <- annotations[source_annotations]
  
  subplots$x$layout$annotations <- append(source, subplots$x$layout$annotations)
  
  subplots
  
}
