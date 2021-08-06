static_plot <- function(plot, static = TRUE){
  #' Title
  #'
  #' @param plot 
  #' @param static 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  
  is_pdf <- try(
    any("pdf_document" %in% rmarkdown::all_output_formats(knitr::current_input())), 
    silent = TRUE
  )
  is_pdf <- (is_pdf == TRUE)

  if (is_pdf | static) {
    orca(plot, file = "temp.png")
  } else {
    plot
  }

}

# Formula to sym ---------------------------------------------------------------
formula_to_sym <- function(x){
  #' Turn a formula into a symbol
  #' 
  #' @details Turn a formula object (as used in \code{plotly} functions for column names) 
  #' into a symbol object (as used in \code{dplyr} functions for column names).  
  #'
  #' @param x a formula that would be passed to plotly functions.  #'
  #'
  #' @examples
  #' # Useful for converting between plotly code and dplyr code
  #' data <- data.frame(
  #'  group = "A"
  #'  x = 1:100,
  #'  y = rnorm(100)
  #' )
  #' 
  #' x_plot <- ~x
  #' y_plot <- ~y
  #' 
  #' plotly::plot_ly(
  #'   data,
  #'   x = x_plot,
  #'   y = y_plot
  #' )
  #' 
  #' x_dplyr <- formula_to_sym(x_plot)
  #' y_dplyr <- formula_to_sym(y_plot)
  #' 
  #' select(data, {{ x_dplyr }}, {{ y_dplyr }})
  
  #  if (is.null(x)) return(x)
  
  if (!rlang::is_formula(x)) {
    stop("x is not a formula")
  }
  
  # take second element of vector - first will be "~"
  x <- as.character(x)[2]
  
  rlang::sym(x)
  
}

# Make colour palette ----------------------------------------------------------
make_colour_palette <- function(n, name){
  #' Make a colour palette of \code{n} colours.
  #' 
  #' @description 
  #' 
  #' Works like \code{RcolorBrewer::brewer.pal} but always returns exactly as many 
  #' colours as requested.
  #'
  #' @param n Number of difference colours in the palette. Any positive integer.
  #' @param name ColorBrewer palette name - see \code{\link[RColorBrewer]{RColorBrewer}}
  #'  and \href{http://www.colorbrewer.org}{ColorBrewer}
  #'
  #' @details
  #' 
  #' Uses \code{grDevices::colorRamp} to produce large colour palettes (more than
  #' 9 colours) by interpolating ones returned by \code{RColorBrewer::brewer.pal}.
  #' Because of this very large colour palettes will be difficult to distinguish.
  #'
  #' @examples
  #' # Make a colour palette with exactly 2 colours 
  #' colours <- make_colour_palette(2, "PuBu")
  #' length(colours) == 2
  #' # Compare to RColorBrewer's minimum of 3 colours
  #' colours_brewer <- RColorBrewer::brewer.pal(2, "PuBu")
  #' length(colours_brewer) == 2
  #' 
  #' # Making palettes with more than 9 colours
  #' colours <- make_colour_palette(11, "PuBu")
  #' length(colours) == 11
  #' colours_brewer <- RColorBrewer::brewer.pal(11, "PuBu")
  #' length(colours_brewer) == 11
  
  # special cases for 0 and 1 otherwise produce brewer.pal palette
  if (n == 0) {
    
    return(NULL)
    
  } else if (n == 1) {
    
    return("#2B8CBE")
    
  } else {
    
    palette <- brewer.pal(n, name = name) %>%
      colorspace::darken() %>%
      suppressWarnings()
    
  }
  
  # brewer.pal will produce a minimum of 3 colours in palette. For a palette of
  # 2 take the first and last
  if (n == 2) {
    
    return(palette[c(1, 3)])
    
  }
  
  # for large palettes interpolate those returned by brewer.pal
  if (n > 9) {
    
    palette <- palette %>%
      colorRampPalette()
    
    return(palette(n))
  }
  
  palette
}


# Factor level function --------------------------------------------------------
level_factors <- function(data, category_type, category, factor_data){
  #' Specify factor level orders
  #' 
  #' @details Use a data.frame to order the levels of factors in another. This
  #' means you can make a csv with all your factor orders specified and not 
  #' worry about having factor orders all over your code.
  #'
  #' @param data data.frame with columns to be turned into factors and ordered
  #' @param category_type column name (unquoted) in data giving the name of 
  #' the overall category group to be joined on the corresponding 
  #' \code{category_type} column in \code{factor_data}
  #' @param category column name (unquoted) to be turned into a factor
  #' @param factor_data data.frame with 3 columns: \code{category_type} and 
  #' \code{category} to join to the columns specified by \code{category_type} 
  #' and \code{category} and an \code{order} column specifying the levels of 
  #' each factor.
  #'
  #' @details 
  #' Returns data untouched if the values in  \code{category_type} and \code{category} 
  #' in \code{factor_data} don't align with the values in the data.
  #'
  #' @examples
  #' # Make the sub_group column into a factor, ordered in reverse alphabetical
  #' # order
  #' data <- data.frame(
  #'   sub_group = sample(letters[10:15], 20, replace = TRUE),
  #'   values = rnorm(20)
  #' )
  #' 
  #' factor_data <- data.frame(
  #'   category_type = "A",
  #'   category = c(letters[10:15]),
  #'   order = 6:1
  #' )
  #' 
  #' arrange(data, sub_group)
  #' 
  #' new_data <- level_factors(data, "A", sub_group, factor_data)
  #' 
  #' arrange(new_data, sub_group)
  #' 
  #' # You can store the orders of many factors in factor_data and apply 
  #' # to different columnns
  #' factor_data <- data.frame(
  #'   category_type = rep(c("A", "B", "C"), times = c(6, 2, 4)),
  #'   category = c(letters[10:15], "B 1", "B 2", LETTERS[23:26]),
  #'   order = c(6:1, 1:2, c(2, 1, 4, 3))
  #' )
  #' 
  #' data <- data.frame(
  #'   A_group = sample(letters[10:15], 20, replace = TRUE),
  #'   B_group = sample(c("B 1", "B 2"), 20, replace = TRUE),
  #'   C_group = sample(LETTERS[23:26], 20, replace = TRUE),
  #'   values = rnorm(20)
  #' )
  #' 
  #' new_data <- data %>%
  #'   level_factors("A", A_group, factor_data) %>%
  #'   level_factors("B", B_group, factor_data) %>%
  #'   level_factors("C", C_group, factor_data)
  #' 
  #' levels(new_data$A_group)
  #' levels(new_data$B_group)
  #' levels(new_data$C_group)
  #' 
  
  category_str <- quo_name(enquo(category))
  category_type_str = quo_name(enquo(category_type))
  
  # check that categories actually turn up in factor_data
  join_check <- semi_join(
    factor_data, 
    data, 
    by = c("category" = category_str, "category_type" = category_type_str)
  ) %>%
    nrow()
  
  # if no categories appear then return original data with warning
  if (join_check == 0) {
    
    warning("No categories from data appear in factor_data")
    return(data)
    
  } else {
    # otherwise do join, make category a factor and reorder based on order
    left_join(
      data,
      factor_data,
      by = setNames(c("category_type", "category"),  c(category_type_str, category_str))
    ) %>%
      mutate(
        {{ category }} := forcats::fct_reorder(factor({{ category }}), order, .fun = min)
      ) %>%
      select(-order)
  }
  
}

# Factor level filter function -------------------------------------------------
level_factors_filt <- function(data, category_type_filt, category, factor_data){
  #' Specify factor level orders
  #' 
  #' @details Use a data.frame to order the levels of factors in another. This
  #' means you can make a csv with all your factor orders specified and not 
  #' worry about having factor orders all over your code.
  #'
  #' @param data data.frame with columns to be turned into factors and ordered
  #' @param category_type_filt character string with 
  #' group name as it appears in \code{factor_data$category_type} of the group name for the 
  #' factors. This is used to filter \code{factor_data} and allows \code{factor_data}
  #' to supply the factor order for multiple columns.
  #' \code{category_type} column in \code{factor_data}
  #' @param category column name (unquoted) to be turned into a factor
  #' @param factor_data data.frame with 3 columns: \code{category_type} and 
  #' \code{category} to join to the columns specified by \code{category_type} 
  #' and \code{category} and an \code{order} column specifying the levels of 
  #' each factor.
  #'
  #' @details 
  #' Returns data untouched if the values in  \code{category_type} and \code{category} 
  #' in \code{factor_data} don't align with the values in the data.
  #'
  #' @examples
  #' # Make the sub_group column into a factor, ordered in reverse alphabetical
  #' # order
  #' data <- data.frame(
  #'   sub_group = sample(letters[10:15], 20, replace = TRUE),
  #'   values = rnorm(20)
  #' )
  #' 
  #' factor_data <- data.frame(
  #'   category_type = "A",
  #'   category = c(letters[10:15]),
  #'   order = 6:1
  #' )
  #' 
  #' arrange(data, sub_group)
  #' 
  #' new_data <- level_factors(data, "A", sub_group, factor_data)
  #' 
  #' arrange(new_data, sub_group)
  #' 
  #' # You can store the orders of many factors in factor_data and apply 
  #' # to different columnns
  #' factor_data <- data.frame(
  #'   category_type = rep(c("A", "B", "C"), times = c(6, 2, 4)),
  #'   category = c(letters[10:15], "B 1", "B 2", LETTERS[23:26]),
  #'   order = c(6:1, 1:2, c(2, 1, 4, 3))
  #' )
  #' 
  #' data <- data.frame(
  #'   A_group = sample(letters[10:15], 20, replace = TRUE),
  #'   B_group = sample(c("B 1", "B 2"), 20, replace = TRUE),
  #'   C_group = sample(LETTERS[23:26], 20, replace = TRUE),
  #'   values = rnorm(20)
  #' )
  #' 
  #' new_data <- data %>%
  #'   level_factors("A", A_group, factor_data) %>%
  #'   level_factors("B", B_group, factor_data) %>%
  #'   level_factors("C", C_group, factor_data)
  #' 
  #' levels(new_data$A_group)
  #' levels(new_data$B_group)
  #' levels(new_data$C_group)
  #' 
  
  category_str <- quo_name(enquo(category))
  
  factor_data <- factor_data %>%
    filter(
      category_type == {{ category_type_filt }}
    ) %>%
    select(-category_type)
  
  # check that categories actually turn up in factor_data
  join_check <- semi_join(
    factor_data, 
    data, 
    by = c("category" = category_str)
  ) %>%
    nrow()
  
  # if no categories appear then return original data with warning
  if (join_check == 0) {
    
    warning("No categories from data appear in factor_data")
    return(data)
    
  } else {
    # otherwise do join, make category a factor and reorder based on order
    left_join(
      data,
      factor_data,
      by = setNames("category",  category_str)
    ) %>%
      mutate(
        {{ category }} := forcats::fct_reorder(factor({{ category }}), order, .fun = min)
      ) %>%
      select(-order)
  }
  
}

# Set axes range ---------------------------------------------------------------
set_y_axis_range <- function(data, y, y_max = NULL) {
  
  y_min <- data %>%
    pull(formula_to_sym({{ y }})) %>%
    min(na.rm = TRUE)
  
  y_min <- min(0, y_min)
  
  y_max_data <- data %>%
    pull(formula_to_sym({{ y }})) %>%
    max(na.rm = TRUE)
  
  y_max <- max(y_max_data, y_max)
  
  y_axis_range <- c(y_min, y_max*1.1)
  
  y_axis_range
  
}

# Replace names ----------------------------------------------------------------
replace_names <- function(data, name_column, name_data) {
  
  name_str <- quo_name(enquo(name_column))
  
  data %>%
    left_join(
      name_data,
      by = setNames("old_name",  name_str)
    ) %>%
    mutate(
      {{ name_column }} := case_when(
        is.na(new_name) ~ {{ name_column }},
        TRUE ~ new_name
      )
    ) %>%
    select(-new_name)
  
}
