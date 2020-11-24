avg_msg_length_barplot <- function(messages_df){
  messages_df %>% 
  select(1:3) %>% 
  mutate(content_word_length = str_count(content, '\\w+')) %>% 
  group_by(sender) %>% 
  summarise(avg_length = round(mean(content_word_length), 2) ) %>% 
  hchart("bar", hcaes(sender, avg_length), 
         dataLabels = list(enabled = TRUE)) %>% 
  hc_title(text= 'Average message length')
  
}
