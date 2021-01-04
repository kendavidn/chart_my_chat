
msg_freq_over_time_lineplot <- function(messages_df){

  messages_df %>% 
    complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                  max(as.Date(timestamp)), 
                                  by = "day")) %>% 
    mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
    mutate(period = as.Date(cut(.$timestamp, breaks = "month"))) %>% 
    group_by(period, sender) %>% 
    summarise(n = sum(counter)) %>% 
    mutate(period = as.factor(period)) %>% 
    mutate(sender = as.factor(sender)) %>% 
    mutate(period = as.Date(period)) %>%
    bind_rows(data.frame(period = seq.Date(min(as.Date(.$period)),
                                           max(as.Date(.$period)), 
                                           by = "month"))) %>% 
    complete(sender, period) %>% 
    mutate(n = replace_na(n, 0)) %>% 
    arrange(period) %>% 
    filter(!is.na(sender)) %>% 
    hchart("area", hcaes(x = period, y = n, group = sender, fill = sender)) %>% 
    hc_plotOptions(series = list(fillOpacity = 0.15)) %>%
    hc_title(text= 'Number of messages sent in each month')
  
}