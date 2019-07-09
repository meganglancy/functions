# Quick slice

fancy_slice <- function(df, group_var, arrange_var, slice_num){
  
  group_var <- dplyr::enquo(group_var)
  arrange_var <- dplyr::enquo(arrange_var)
  
  df %>% 
    dplyr::group_by(!!group_var) %>% 
    dplyr::arrange(!!arrange_var) %>% 
    dplyr::slice(slice_num) %>% 
    dplyr::ungroup()
  
}