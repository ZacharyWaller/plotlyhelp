# Take some data in and save it with some standardised column names
# Figure name just needs to be a name - it will automatically be saved to the 
# outputs folder with ".csv" appended
save_data <- function(data, x = NULL, y = NULL, breakdown = NULL, dropdown = NULL, subplot = NULL, bar_line = NULL, figure_name = "test") {
  
  x <- formula_to_sym(x)
  y <- formula_to_sym(y)
  breakdown <- formula_to_sym(breakdown)
  dropdown <- formula_to_sym(dropdown)
  subplot <- formula_to_sym(subplot)
  bar_line <- formula_to_sym(bar_line)
  
  data <- data %>%
    select(
      "x" = !! x, 
      "y" = !! y, 
      "breakdown" = !! breakdown, 
      "dropdown" = !! dropdown, 
      "subplot" = !! subplot, 
      "bar_line" = !! bar_line
    ) %>%
    # if we've done any fiddling around to add rows we'll remove them
    filter(
      !is.infinite(y)
    )
  
  figure_name <- paste0("plot_data/", figure_name, ".csv")
  
  write_csv(data, figure_name)
  
}
