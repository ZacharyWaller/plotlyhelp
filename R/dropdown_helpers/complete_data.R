# This is a slightly nasty bit of code to solve a nasty plotly issue
# When making subplots with dropdowns you must have the same number of breakdowns
# in every subplot you want to display at once.
# e.g. if you have two plots and the top one has "higher" ,"lower" and "same as"
# while your bottom plot only has "higher" everything will go awry when you select 
# a new dropdown. The reason for this is that the dropdown button is taken from the 
# first plot in the subplots and is then applied to all the other plots. This is 
# mixed with the fact that making plots visible/invisible you are actually turning
# on/off traces and new splits/colours of data are counted as new traces.
# What this means in our example is that the first dropdown option will be turning
# on the first 3 traces of each plot, but there only 2 traces on the second subplot
# This messes everything up and you end up with wild plots!
# 
# If you don't believe me turn off complete_data in make_subplots_dropdowns and
# look at the plots you get!
#
# The solution is to pad our data out with more rows, with the right breakdown values
# so every subplot has the same number of traces
complete_data <- function(data, x, y, dropdown, subplot, breakdown, bar_line) {
  
  warning("Completing data. For bar plots, setting barmode to 'stack' or 'relative' will make this less obvious")
  
  # add in new rows for each dropdown complete the breakdowns. These will have 
  # no x, bar_line, or y value though - which we need for plotting
  complete <- data %>%
    complete(nesting( !!dropdown, !! breakdown), !!subplot)
  
  # take the new rows we just made
  new_rows_nas <- complete %>%
    anti_join(data, by = intersect(colnames(.), colnames(data))) %>%
    select(!!dropdown, !! breakdown, !!subplot)
  
  cols <- c()
  
  # join to the old data to get the x, bar_line and y values.
  # x will be last in a group, y will be Inf and bar_line will be whatever the bar_line
  # value is for that x value (this means it's not obvious there's a dummy value)
  new_row <- new_rows_nas %>%
    select({{ dropdown }}, {{ subplot }}) %>%
    left_join(data, by = intersect(colnames(.), colnames(data))) %>%
    group_by({{ dropdown }}, {{ subplot }}) %>%
    slice_min(!! x ) %>%
    ungroup() %>%
    select(!! dropdown, !! subplot, !! bar_line, !! x) %>%
    # Neat little trick I stumbled upon - plotly is dumb with NAs and tries to plot
    # things and also gives wacky results. Setting to 0 could work, but also 
    # displays a bit of error bar. Using Inf or -Inf doesn't plot anything 
    # while treating the value normally in every other sense.
    mutate(
      !! y := Inf
    ) %>%
    left_join(new_rows_nas, by = intersect(colnames(.), colnames(new_rows_nas)))
  
  bind_rows(data, new_row)

}
