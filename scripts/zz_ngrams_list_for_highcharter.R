bigrams <- messages_df %>%   
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
      unnest_tokens(output = gram, input = text, token = "ngrams", n = 2) %>% 
      separate(gram, c("word1", "word2"), sep = " ") %>% 
      filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
      transmute(gram = paste(word1, word2)) %>% 
      count(gram, sort = T)  # %>%
    #      top_n_sample_ties(nb_to_samp = 20) 
  }) %>% 
  bind_rows(.id = "sender") %>% 
  group_by(gram) %>% mutate(n_total = sum(n)) %>% ungroup() %>% 
  slice_max(n = 15, order_by= n_total, with_ties = T)


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
  group_by(gram) %>% mutate(n_total = sum(n)) %>% ungroup() %>% arrange(-n_total) %>% 
  slice_max(n = 15, order_by =n_total,with_ties = T)

gram_series_for_hc <- 
  bind_rows(bigrams, trigrams) %>% 
  mutate(bigram = ifelse(str_count(gram, '\\w+') == 2, gram, NA )) %>% 
  mutate(trigram = ifelse(str_count(gram, '\\w+') == 3, gram, NA )) %>% 
  # drop bigrams that also feature in a trigram
  mutate(bigram_check = ifelse(str_detect(string = paste( na.omit(.$trigram), collapse = "/"), pattern = bigram), "dup.", "ok" )) %>% 
  filter(bigram_check!= "dup." | is.na(bigram_check))  %T>%
  # group_by(sender) %>% sample_n(15, wt = n_total) %>% ungroup() %T>%  # for debgugging
  # expand so that each phrase is associated with all users, even if they never used it (in which case their n will be 0)
  {
    expand(., sender, gram) ->> permut
  } %>%
  right_join(permut) %>%
  mutate(n = replace_na(n, 0)) %>%
  # bunch together rows with the same gram
  group_by(sender) %>% arrange(gram, sender) %>% ungroup() %>%
  # fill in missings in "n_total" column. 
  group_by(gram) %>% mutate(n_total = first(na.omit(n_total))) %>% ungroup() %>%
  # then take the top most common grams (grouped so you don't catch an n-gram for only one person)
  pivot_wider(id_cols = c(gram, n_total), values_from = n,names_from = sender) %>%
  top_n_sample_ties(rank_column = n_total, nb_to_samp = 20) %>%
  arrange(-n_total) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Create highcharts plot with possible multiple senders  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# create vector of people in the chat
senders <- names(gram_series_for_hc)[-c(1,2)]

# initialize list
out_text <- list()

# function that pastes the number of data lists necessary for plot
# final list is different since it won't have a separating comma
make_highchart_list_multiple_senders <- function(senders){
  for (i in 3:(length(senders)+2)  ){
    out_text[i] <- paste0("list(name= names(gram_series_for_hc)[", i, "] ,", 
               " data= gram_series_for_hc[", i, "] %>% pull()),")
  }
  
  for (i in (length(senders)+2)  ){
    out_text[i] <- paste0("list(name= names(gram_series_for_hc)[", i, "] ,", 
                " data= gram_series_for_hc[", i, "] %>% pull())")
  }

  return(out_text %>% 
           flatten_chr() %>% 
           paste(collapse = " "))
}

final_expr_for_plot <- paste("highchart() %>%
  hc_chart(type= 'bar') %>%
  hc_title(text= 'Phrases used most frequently')%>%
  hc_xAxis(list(categories=gram_series_for_hc[1] %>% pull() , labels=list(step= 1)))%>%
  hc_plotOptions(series=list(stacking= 'normal'))%>%
  hc_series(", make_highchart_list_multiple_senders(senders), ")")

eval(str2expression(final_expr_for_plot))



