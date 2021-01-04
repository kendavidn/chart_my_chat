
whatsapp_txt_to_clean_df <- function(whatsapp_txt){
  whatsapp_txt %>% 
    rwa_read() %>% 
    filter(!is.na(author)) %>% 
    as_tibble() %>% 
    mutate(author = as.character(author)) %>% 
    mutate(author = ifelse(author == ".", "Kene", author)) %>% 
    mutate(author = ifelse(str_detect(author, "Ojay"), "Ojay", author)) %>% 
    mutate(author = ifelse(str_detect(author, "61"), "Ojay", author)) %>% 
    mutate(author = ifelse(str_detect(author, "Aka"), "Aka", author)) %>% 
    mutate(author = ifelse(str_detect(author, "Elo"), "Elo", author)) %>% 
    mutate(author = ifelse(str_detect(author, "Olive"), "Olive", author)) %>% 
    mutate(text = ifelse(str_detect(text, "Ayodele"), "", text)) %>% 
    mutate(text = ifelse(str_detect(text, "deleted this message"), "", text)) %>% 
    mutate(text = ifelse(str_detect(text, "https"), "", text)) %>% 
    select(author, time, text) %>% 
    rename(timestamp = time, 
           sender = author, 
           content = text) %>% 
    mutate(content  = iconv(content, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
    filter(!str_detect(content, "Media omitted")) %>% 
    filter(!str_detect(content, "This message was deleted"))
}