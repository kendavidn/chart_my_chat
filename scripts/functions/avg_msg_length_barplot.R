avg_msg_length_barplot <- function(messages_df){
  messages_df %>% 
  select(1:3) %>% 
  mutate(content_word_length = str_count(content, '\\w+')) %>% 
  group_by(sender) %>% 
  summarise(avg_length = round(mean(content_word_length), 2) ) %>% 
  #mutate(sender = fct_reorder(sender, -avg_length)) %>% 
  hchart("bar", hcaes(sender, avg_length, group = sender, fill = sender), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text= 'Average message length') %>% 
  hc_yAxis(title = list(text = "Number of words")) %>%
  hc_xAxis(title = list(text = "")) %>% 
    hc_legend(
      align= 'right',
      verticalAlign= 'top',
      layout= 'vertical',
      x= 0,
      y= 100) %>%
  hc_plotOptions(series=list(stacking= 'normal'))
    
  
}
