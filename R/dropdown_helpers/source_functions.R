make_data_source <- function(data, text, x, y) {
  
  if (source_text == "") {
    
    text <- ""
    
  } else {
    
    text <- paste0("<b>Source:</b> ", source_text)
    
  }
  
  source_list <- list(
    text = text,
    x = 1,
    y = source_y,
    xanchor = "right",
    yanchor = "middle",
    xref = "paper",
    yref = "paper",
    align = "right",
    valign = "top",
    showarrow = FALSE,
    # this label does nothing but it's helpful when distinguishing between annotations
    label = "source"
  )
  
  source_list
  
}

make_extra_labels <- function(data, text, x, y, xnudge = 0, ynudge = 0) {
  
  text <- formula_to_sym(text)
  x <- formula_to_sym(x)
  y <- formula_to_sym(y)
  
  if (is.null(text)) {
    return(NULL)
  }
  
  data <- data %>%
    filter(!is.na({{ text }}))
    
  text <- pull(data, {{ text }})
  
  text <- stringr::str_wrap(text, width = 15)
  
  x <- pull(data, {{ x }})
  y <- pull(data, {{ y }})
  
  annotations <- pmap(
    list(
      text = text,
      x = x,
      y = y,
      xnudge = xnudge,
      ynudge = ynudge
    ),
    .f = function(text, x, y, xnudge, ynudge){
      list(
        text = as.character(text),
        x = as.numeric(x) + xnudge,
        y = as.numeric(y) + ynudge,
        yanchor = "bottom",
        xanchor = "middle",
        align = "middle",
        valign = "top",
        bordercolor = "#c7c7c7",
        borderwidth = 2,
        borderpad = 4,
        opacity = 0.8,
        showarrow = FALSE,
        xref = "x",
        yref = "y",
        # this label does nothing but it's helpful when distinguishing between annotations
        label = "extras"
      )
    }
  )
  
  annotations
  
}
