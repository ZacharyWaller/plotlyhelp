make_unique_values <- function(data, group, x, x_name_orig) {
  
  unique_values <- data %>%
    distinct({{ group }}, {{ x }}) %>%
    group_by({{ x }}) %>%
    arrange({{ group }}) %>%
    mutate(n = row_number()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(
      unique_x = paste0(paste0(rep("<b></b>", n - 1), collapse = ""), {{ x }})
    ) %>%
    ungroup() %>%
    select(-n)
  
  
  data %>%
    left_join(
      unique_values, by = intersect(colnames(.), colnames(unique_values))
    ) %>%
    arrange(
      {{ group }}, {{ x }}
    ) %>%
    mutate(
      {{ x_name_orig }} := {{ x }},
      {{ x }} := unique_x
    ) %>%
    select(-unique_x) %>%
    mutate(
      {{ x }} := factor({{ x }}, levels = unique({{ x }}))
    )
  
}
