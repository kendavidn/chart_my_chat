
word_count_donutplot <- function(messages_df){
  
  x <- c("Sender:", "Count:")
  y <- c("{point.sender}", "{point.count}")
  
  tltip <- tooltip_table(x, y)
  
messages_df %>% 
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "words") %>% 
      summarise(count = n())
  }) %>% 
  bind_rows(.id = "sender") %>% 
  hchart("pie", hcaes(name = sender, y = count ), 
         innerSize = "40%", 
         showInLegend = TRUE, 
         dataLabels = list(enabled = FALSE)) %>% 
  hc_title(text= 'Total number of words used') %>% 
  hc_tooltip(useHTML = TRUE,
             headerFormat = "", 
             pointFormat = tltip) 
}