# Group records into 'episodes' and assign episode id
# 2019-07-10

ep_calc <- function(df, group_var, date_var, ep_limit) {
  group_var <- dplyr::enquo(group_var) # Create quosure   
  date_var <- dplyr::enquo(date_var) # Must be in 'date' format
  
  df %>% 
    dplyr::group_by(!!group_var) %>%        # Use !! to unquote the quosure
    dplyr::arrange(!!group_var, !!date_var) %>% 
    dplyr::mutate(
      datediff = !!date_var - dplyr::lag(!!date_var, default = min(!!date_var)),
      rep_ep = case_when(
        !!date_var == lag(!!date_var) ~ 0L,
        !!date_var == min(!!date_var) ~ 1L,
        datediff > ep_limit ~ 1L, # ep_limit is in days
        TRUE ~ 0L)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(ep_ref = cumsum(rep_ep)) %>% 
    dplyr::select(-datediff, -rep_ep)
}
