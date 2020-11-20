bigrams <- messages_df %>%   
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "ngrams", n = 2, ) %>% 
      separate(gram, c("word1", "word2"), sep = " ") %>% 
      filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
      transmute(gram = paste(word1, word2)) %>% 
      count(gram, sort = T)  # %>%
    #      top_n_sample_ties(nb_to_samp = 20) 
  }) %>% 
  bind_rows(.id = "sender") %>% 
  group_by(gram) %>% mutate(n_total = sum(n)) %>% ungroup()


split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "ngrams", n = 2, ) %>% 
      separate(gram, c("word1", "word2"), sep = " ") %>% 
      filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
      transmute(gram = paste(word1, word2)) %>% 
      count(gram, sort = T)  # %>%
    #      top_n_sample_ties(nb_to_samp = 20) 
  })



trigrams <-  
  messages_df %>% 
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "ngrams", n = 3) %>% 
      count(gram, sort = T)
  }) %>% 
  bind_rows(.id = "sender") %>% 
  group_by(gram) %>% mutate(n_total = sum(n)) %>% ungroup() 


p1 <- bind_rows(bigrams, trigrams) %>% 
  top_n_sample_ties(rank_column = n_total, nb_to_samp = 60) %>% 
  mutate(gram = fct_reorder(gram, n_total)) %>% 
  ggplot(aes(x = gram, y = n, fill = sender)) + 
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()

plotly::ggplotly(p1)

gram_series_for_hc <- 
  bind_rows(bigrams, trigrams) %>%
  group_by(sender) %>% sample_n(15, wt = n_total) %>% ungroup() %T>%
  # expand so that each phrase is associated with all users, even if they never used it
  # (in which case their n will be 0)
  {expand(., sender, gram) ->> permut} %>%
  right_join(permut) %>%
  mutate(n = replace_na(n, 0)) %>%
  # bunch together rows with the same gram
  group_by(sender) %>%
  arrange(gram, sender) %>%
  ungroup() %>%
  # fill in missings in "n_total" column
  group_by(gram) %>%
  mutate(n_total = zoo::na.locf(n_total)) %>%
  ungroup() %>%
  # then take the top most common grams (grouped so you don't catch an n-gram for only one person)
  spread(key = )
  
    group_by(sender) 

# %>% slice_max()
  
  filter(df.g, rank(x, ties.method="first")==1)

  ungroup() %>%
  #   # extract wordlist and store. Arranging necessary since pulling out the column to a naked list for highchart
  arrange(-n_total, sender) %T>%
  {
    unique(.$gram) ->> trigram_wordlist_for_hc
  } %>%
  # condense into nested list format needed for highcharter
  select(-n_total) %>%
  group_by(sender) %>%
  do(data = .$n) %>%
  highcharter::list_parse() 


highchart() %>%
  hc_chart(type= 'bar')%>%
  hc_title(text= 'Phrases used most frequently')%>%
  hc_xAxis(list(categories=trigram_wordlist_for_hc, labels=list(step= 1)))%>%
  hc_plotOptions(series=list(stacking= 'normal'))%>%
  hc_series(
    list(name= gram_series_for_hc[[1]][1] %>% flatten_chr(),
         data= gram_series_for_hc[[1]][2] %>% flatten_dbl()),
    
    list(name= gram_series_for_hc[[2]][1] %>% flatten_chr(),
         data= gram_series_for_hc[[2]][2]  %>% flatten_dbl()))













messages_df %>%   
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "ngrams", n = 2, ) %>% 
      separate(gram, c("word1", "word2"), sep = " ") %>% 
      filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
      transmute(gram = paste(word1, word2)) %>% 
      count(gram, sort = T)  # %>%
    #      top_n_sample_ties(nb_to_samp = 20) 
  }) %>% 
  bind_rows(.id = "sender") 


messages_df %>%   
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "ngrams", n = 2) # %>% 
      # separate(gram, c("word1", "word2"), sep = " ") %>% 
      # filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
      # transmute(gram = paste(word1, word2)) %>% 
      # count(gram, sort = T)  # %>%
    #      top_n_sample_ties(nb_to_samp = 20) 
  }) %>% 
  bind_rows(.id = "sender") 



test <- messages_df$content %>%
  paste(collapse = ' ') %>%
  tokenizers::tokenize_ngrams(n = 5, n_min = 2) %>% flatten_chr() %>% 
  data_frame() %>% rename(text = 1) %>% 
  group_by(text) %>% count(sort = T) %>% 
  mutate(bigram = ifelse(str_count(text, '\\w+') == 2, text, NA )) %>% 
  mutate(trigram = ifelse(str_count(text, '\\w+') == 3, text, NA )) %>% 
  mutate(tetragram = ifelse(str_count(text, '\\w+') == 4, text, NA )) %>% 
  mutate(pentagram = ifelse(str_count(text, '\\w+') == 5, text, NA )) %>% 
  mutate(pentagram = ifelse(str_count(text, '\\w+') == 5, text, NA )) %>% 
  mutate(bigram_check = ifelse(str_detect(string = bigram, pattern = paste( .$trigram, collapse = "--") ), "duplicate", "good" ))
  
  arrange(-n)
  
# testdf   <- 
  messages_df$content %>%
    head(3) %>% 
    paste(collapse = ' ') %>%
    tokenizers::tokenize_ngrams(n = 3, n_min = 2) %>% flatten_chr() %>% 
    data_frame() %>% rename(text = 1) %>% 
    group_by(text) %>% count(sort = T) %>% 
    mutate(bigram = ifelse(str_count(text, '\\w+') == 2, text, NA )) %>% 
    mutate(trigram = ifelse(str_count(text, '\\w+') == 3, text, NA )) %>% 
    # mutate(bigram_check = ifelse(str_detect(string = paste( .$trigram, collapse = "--"), pattern = bigram ), "dup.", "ok" ))
    mutate(bigram_check = ifelse(str_detect(string = paste( na.omit(.$trigram), collapse = "/"), pattern = bigram), "dup.", "ok" )) %>% 
    filter(bigram_check != "dup." | is.na(bigram_check))
  
  testdf %>% 
  filter(str_detect(string = bigram, pattern = paste( na.omit(testdf$trigram), collapse = "/")))
    
    
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}




astring <- "row, row your boat, float your boat"

astring %>% 
  tokenizers::tokenize_ngrams(n = 3, n_min = 2) %>% data.frame() %>% 
  rename(text = 1) %>% 
  group_by(text) %>% count() %>% ungroup() %>% 
  mutate(bigram = ifelse(str_count(text, '\\w+') == 2, text, NA )) %>% 
  mutate(trigram = ifelse(str_count(text, '\\w+') == 3, text, NA )) %>% 
  # mutate(bigram_check = ifelse(str_detect(string = paste( .$trigram, collapse = "--"), pattern = bigram ), "dup.", "ok" ))
  mutate(bigram_check = ifelse(str_detect(string = paste( na.omit(.$trigram), collapse = "/"), pattern = bigram), "dup.", "ok" )) %>% 
  mutate(trigram_count = ifelse(!is.na(trigram), n, NA )) %>% 
  mutate(bigram_check_paste = ifelse(str_detect(string = paste( na.omit(.$trigram), collapse = "/"), pattern = bigram), bigram , "ok" )) %>% 
  # if any two-word string in the trigram is found in a bigram, paste the offending bigram in a new column
  mutate(trigram_split = tokenize_ngrams(trigram, n = 2)) %>% 
  unnest(cols = trigram_split) %>% 
  mutate(bigram = ifelse(is.na(bigram), trigram_split, bigram )  ) %>% 
  group_by(bigram) %>% arrange(desc(text)) %>% 
  mutate(trigram = zoo::na.locf(trigram)) %>% 
  mutate(n_plus = ifelse(bigram_check == "dup.", n - 0.1, n)) %>% 
  mutate(n_plus = ifelse(is.na(bigram_check), n , n_plus)) %>% 
  group_by(trigram) %>% top_n_sample_ties(rank_column = n_plus, nb_to_samp = 1)
  
messages_df$content %>%
  # tail(10) %>% 
  paste(collapse = ' ') %>% 
  tokenizers::tokenize_ngrams(n = 3, n_min = 2) %>% data.frame() %>% 
  rename(text = 1) %>% 
  group_by(text) %>% count() %>% ungroup() %>% 
  mutate(bigram = ifelse(str_count(text, '\\w+') == 2, text, NA )) %>% 
  mutate(trigram = ifelse(str_count(text, '\\w+') == 3, text, NA )) %>% 
  # mutate(bigram_check = ifelse(str_detect(string = paste( .$trigram, collapse = "--"), pattern = bigram ), "dup.", "ok" ))
  mutate(bigram_check = ifelse(str_detect(string = paste( na.omit(.$trigram), collapse = "/"), pattern = bigram), "dup.", "ok" )) %>% 
  mutate(trigram_count = ifelse(!is.na(trigram), n, NA )) %>% 
  mutate(bigram_check_paste = ifelse(str_detect(string = paste( na.omit(.$trigram), collapse = "/"), pattern = bigram), bigram , "ok" )) %>% 
  # if any two-word string in the trigram is found in a bigram, paste the offending bigram in a new column
  mutate(trigram_split = tokenize_ngrams(trigram, n = 2)) %>% 
  unnest(cols = trigram_split) %>% 
  mutate(bigram = ifelse(is.na(bigram), trigram_split, bigram )  ) %>% 
  group_by(bigram) %>% arrange(desc(text)) %>% 
  mutate(trigram = zoo::na.locf(trigram, na.rm = FALSE)) %>% 
  mutate(n_plus = ifelse(bigram_check == "dup.", n - 0.1, n)) %>% 
  mutate(n_plus = ifelse(is.na(bigram_check), n , n_plus)) %>% 
  group_by(trigram) %>% top_n_sample_ties(rank_column = n_plus, nb_to_samp = 1) %>% ungroup() %>% 
  arrange(-n_plus)








