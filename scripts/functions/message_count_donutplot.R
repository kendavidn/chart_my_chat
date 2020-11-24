
message_count_donutplot <- function(messages_df) {
  
messages_df %>% 
  group_by(sender) %>% 
  summarise(count = n()) %>% 
  hchart("pie", hcaes(name = sender, y = count ), 
         innerSize = "40%", 
         showInLegend = TRUE, 
         dataLabels = list(enabled = FALSE)) %>% 
  hc_title(text= 'Total number of texts sent')
  
}