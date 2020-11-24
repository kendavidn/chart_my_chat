
word_count_donutplot <- function(messages_df){
messages_df %>% 
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "words") %>% 
      count()
  }) %>% 
  bind_rows(.id = "sender") %>% 
  hchart("pie", hcaes(name = sender, y = n ), 
         innerSize = "40%", 
         showInLegend = TRUE, 
         dataLabels = list(enabled = FALSE)) %>% 
  hc_title(text= 'Total number of words used')
}