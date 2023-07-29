

library(tidyverse)
library(sf)
library(scales)
library(units)
library(arrow)
library(geoarrow)
library(sfarrow)
options(scipen = 999)

# Aggregation function ----------------------------------------------------

#' @param data dataframe (must not contain columns named 'group_var' or 'group_val')
#' @param group_by_cols vector of columns to group by but not stack
#' format c('county_code','state_code','census_region','coastal_state')
#' @param crosstab_cols vector of categorical columns (or list of column names with vector with factor order) in wide data to stack into vertical crosstabs organized into 'group_var' (col_name), 'group_val' (col_value)
#' format c('demo_educ_var','demo_race_var','demo_income_var','topline_var')
#' format list('demo_educ_var' = c('BA+','sub-BA'),'demo_race_var' = c('White','Black','Latino','Asian'),'demo_income_var' = c('Under $50K','$50K-100K','$100K+'),'topline_var' = c('Topline'))
#' @param sum_cols vector of numerical columns to summarize / collapse by crosstab_cols and group_by_cols
#' format c('population_var', 'income_var')
#' @param divide_cols named list of named vectors naming columns to divide into new columns using list name
#' format list('varname1' = c('var_numerator1','var_denominator1'), 'varname2' = c('var_numerator2','var_denominator2'))
#' @param transform_cols vector of columns to transform into new columns named in format '{.col}_{.fn_name}' with {.fn_name} being name in transform_func_list
#' format c('population_var', 'income_var')
#' @param transform_func_list named list with functions to apply to transform_cols
#' format list(log_col = log10, sqrt_col = sqrt)
#' @param group_by_agg_cols_list named list of vectors containing columns to group by and then aggregate into new columns named in format '{.col}_group_by_{.fn_name}_{.group_name}
#' format list('state' = c('state_code'), 'coast_region' = c('census_region', coastal_state))
#' @param group_by_agg_func_list named list with functions to apply to group_by_agg_cols_list 
#' format list(agg_sum = sum, agg_mean = mean)
#' @param agg_cols vector of columns to aggregate by group_by_agg_cols_list
#' format c('population_var', 'income_var')
#' 
#' @return dataframe 
generate_crosstabs <- function(data, 
                               group_by_cols, crosstab_cols,
                               sum_cols, divide_cols, 
                               transform_cols, transform_func_list,
                               group_by_agg_cols_list, group_by_agg_func_list, agg_cols) { 
  
  data <- data %>% 
    group_by_at(c(crosstab_cols, group_by_cols)) %>%
    summarize_at(vars(c(all_of(sum_cols))), list(sum)) %>%
    ungroup() 
  
  data <- purrr::map_dfr(.x = crosstab_cols, .f = ~ data %>%
                           group_by_at(c(.x, group_by_cols)) %>%
                           summarize_at(vars(c(all_of(sum_cols))), list(sum)) %>% 
                           ungroup() %>%
                           mutate(group_var = paste0(.x)) %>%
                           rename(group_val = all_of(.x)))
  
  data <- data %>%
    relocate(any_of(c(c('group_var','group_val'), group_by_cols, sum_cols) %>% unique()))
  
  if (length(divide_cols) > 0) {
    quotient_names = list_flatten(divide_cols) |> names()
    numerator_cols = list_flatten(divide_cols) |> transpose() |> pluck(1) |> unname() |> as_vector()
    denominator_cols = list_flatten(divide_cols) |> transpose() |> pluck(2)  |> unname() |> as_vector()
    data[quotient_names] <- data[numerator_cols]/data[denominator_cols] 
    data <- data %>% 
      mutate(across(.cols = all_of(quotient_names), .fns = ~ replace_na(na_if(na_if(na_if(.x, NaN), Inf), -Inf), 0), .names = "{.col}"))
    
    data <- data %>%
      relocate(any_of(c(c('group_var','group_val'), group_by_cols, quotient_names) %>% unique()))
  } 
  
  if (length(transform_cols) > 0) {
    data <- data %>%
      mutate(across(.cols = all_of(transform_cols), .fns = transform_func_list, .names = "{.col}_{.fn}"))
    
    data <- data %>%
      relocate(any_of(c(c('group_var','group_val'), group_by_cols, starts_with(transform_cols)) %>% unique()))
  }
  
  data <- data %>%
    arrange(group_var, !!!group_by_cols, group_val)
  
  if (length(group_by_agg_cols_list) > 0) {
    for (i in seq_along(group_by_agg_cols_list)) {
      group_agg_cols <- group_by_agg_cols_list %>% pluck(i)
      group_agg_name <- group_by_agg_cols_list %>% names %>% pluck(i)
      data <- data %>%
        group_by_at(c('group_var',group_agg_cols)) %>%
        mutate(across(.cols = all_of(agg_cols), .fns = group_by_agg_func_list, .names = paste0("agg_{.fn}_{.col}_group_by_",group_agg_name))) %>%
        ungroup()
    }
  }
  return(data)
}


# data = df_combined_prep
# group_by_cols = c("continent", "class_urban_hierarchy")
# crosstab_cols = c("k_0")
# sum_cols = sum_cols
# divide_cols = divide_cols
# transform_cols = transform_cols
# transform_func_list = list(log10 = log_10)
# group_by_agg_cols_list = list('continent' = c("continent"), 'continent_4way' = c("continent", "class_urban_hierarchy"))
# group_by_agg_func_list = list(sum = sum, share = share)
# agg_cols = c('landscan_population_un', 'worldpop_population_un')


