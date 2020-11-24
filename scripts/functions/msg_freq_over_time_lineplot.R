
msg_freq_over_time_lineplot <- function(messages_df){

messages_df %>% 
  complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                max(as.Date(timestamp)), 
                                by = "day")) %>% 
  mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
  mutate(period = as.Date(cut(.$timestamp, breaks = "month"))) %>% 
  group_by(period) %>% 
  summarise(n = sum(counter)) %>% 
  arrange(period) %>% 
  hchart("areaspline", hcaes(x = period, y = n)) %>% 
  hc_title(text= 'Number of messages sent in each month')
  
}