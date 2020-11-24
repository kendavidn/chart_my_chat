
json_list_to_clean_df <- function(json_list){
  json_list %>% 
.["messages"] %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rename(sender = 1, timestamp_ms = 2, content = 3 ) %>% 
  mutate(content  = iconv(content, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  filter(!str_detect(content, "sent an attachment")) %>% 
  filter(!str_detect(content, "missed a call from")) %>% 
  filter(!str_detect(content, "missed a video call from")) %>% 
  filter(!str_detect(content, "video chat ended")) %>% 
  mutate(timestamp = ms_to_date(timestamp_ms))
}