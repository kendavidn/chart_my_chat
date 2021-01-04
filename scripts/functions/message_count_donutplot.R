
message_count_donutplot <- function(messages_df) {
  
  x <- c("Sender:", "Count:")
  y <- c("{point.sender}", "{point.count}")
  
  tltip <- tooltip_table(x, y)
  
messages_df %>% 
  group_by(sender) %>% 
  summarise(count = n()) %>% 
  hchart("pie", hcaes(name = sender, y = count ), 
         innerSize = "40%", 
         showInLegend = TRUE, 
         dataLabels = list(enabled = FALSE)) %>% 
  hc_title(text= 'Total number of texts sent') %>% 
  hc_tooltip(useHTML = TRUE,
             headerFormat = "", 
             pointFormat = tltip) 
  
}



