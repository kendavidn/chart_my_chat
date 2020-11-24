
msg_freq_weekly_radialplot <- function(messages_df){
  
    plot <- 
      messages_df %>%
      select(1:3, timestamp) %>% 
      complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                   max(as.Date(timestamp)), 
                                   by = "day")) %>% 
      # some "messages" dont have senders. e.g. someone called
      mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
      mutate(day = as.Date(cut(.$timestamp, breaks = "day"))) %>% 
      mutate(day_of_week = lubridate::wday(day,label = T)) %>% 
      group_by(sender, day_of_week) %>% 
      summarise(n = sum(counter)) %>% 
      arrange(day_of_week) %>% 
      filter(!is.na(sender)) %>% 
      hchart("area", hcaes(x = day_of_week, y = n, group = sender, fill = sender) ) %>% 
      hc_chart(polar = T) %>% 
      hc_plotOptions(area = list(fillOpacity = 0.15)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = "")) %>% 
      hc_title(text= 'Messages per day of the week')
    
    
    return(plot)
  
}