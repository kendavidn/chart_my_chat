

top_ngrams_plot <- function(messages_df, max_nb = 20) {
  
  # max_nb = 20
  
  count_grams <- function(df, slice_nb = 10, gram_no){ 
    df %>% 
      split(.$sender) %>%
      map(.f = function(df) {
        df$content %>%
          paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
          unnest_tokens(output = gram, input = text, token = "ngrams", n = gram_no) %>% 
          # for bigrams, filter out stopwords
          {if (gram_no == 2) 
            separate(data = . , gram, c("word1", "word2"), sep = " ") %>% 
              filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
              transmute(gram = paste(word1, word2)) else .} %>% 
          count(gram, sort = T) 
      }) %>% 
      bind_rows(.id = "sender") %>% 
      group_by(gram) %>% mutate(n_total = sum(n)) %>% ungroup() %>% 
      slice_max(n = slice_nb, order_by= n_total, with_ties = T)
  }  
  
  bigrams <- messages_df %>% count_grams(gram_no = 2)
  trigrams <- messages_df %>% count_grams(gram_no = 3)
  tetragrams <- messages_df %>% count_grams(gram_no = 4)
  pentagrams <- messages_df %>% count_grams(gram_no = 5)
  
  
gram_series_for_hc <- 
    bind_rows(bigrams, trigrams) %>% 
    bind_rows(tetragrams) %>% 
    bind_rows(pentagrams) %>% 
    mutate(bigram = ifelse(str_count(gram, '\\w+') == 2, gram, NA )) %>% 
    mutate(bigram_count = ifelse(!is.na(bigram), n, NA )) %>% 
    mutate(trigram = ifelse(str_count(gram, '\\w+') == 3, gram, NA )) %>% 
    mutate(trigram_count = ifelse(!is.na(trigram), n, NA )) %>% 
    mutate(tetragram = ifelse(str_count(gram, '\\w+') == 4, gram, NA )) %>% 
    mutate(tetragram_count = ifelse(!is.na(tetragram), n, NA )) %>% 
    mutate(pentagram = ifelse(str_count(gram, '\\w+') == 5, gram, NA )) %>% 
    mutate(pentagram_count = ifelse(!is.na(pentagram), n, NA )) %T>% 
    {
      expand(., sender, gram) ->> permut
    } %>%
    inner_join(permut) %>%
    mutate(n = replace_na(n, 0)) %>%
    # bunch together rows with the same gram
    group_by(sender) %>% arrange(gram, sender) %>% ungroup() %>% 
    drop_ngrams_with_higher_val_grams() %>%
  # fill in missings in "n_total" column. 
    group_by(gram) %>% mutate(n_total = first(na.omit(n_total))) %>% ungroup() %>%
    # then take the top most common grams (grouped so you don't catch an n-gram for only one person)
    pivot_wider(id_cols = c(gram, n_total), values_from = n,names_from = sender) %>%
    top_n_sample_ties(rank_column = n_total, nb_to_samp = max_nb) %>%
    # drop people who have no utterances from the final list
    select_if( ~ !is.numeric(.) || sum(., na.rm =T) != 0 ) %>% 
    arrange(-n_total) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Create highcharts plot with possible multiple senders  ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # create vector of people in the chat
  senders <- names(gram_series_for_hc)[-c(1,2)]
  
  # initialize list for loop
  out_text <- list()
  
  # function that pastes the number of data lists necessary for plot.
  make_highchart_list_multiple_senders <- function(senders){
    for (i in 3:(length(senders)+2)  ){
      out_text[i] <- paste0("list(name= names(gram_series_for_hc)[", i, "] ,", 
                            " data= gram_series_for_hc[", i, "] %>% pull()),")
    }
    
    # final list is separate since it won't have a separating comma
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
  
  plot <- eval(str2expression(final_expr_for_plot))
  
  return(plot)
  
}





drop_ngrams_with_higher_val_grams <- function(gram_series_for_hc = gram_series_for_hc){
  
  # define function to be used loops
  # function checks checks that the being-iterated-over string 
  pull_max_matched_ngram <- function(df, string, select_col, pattern){
    
    count <- df %>% 
      filter(str_detect(string = {{string}}, pattern = pattern)) %>% 
      select({{select_col}}) %>% pull()
    
    if(length(count) == 0){
      out <- 0
    } else {
      out <- max(count, na.rm = T)
    }
    return(out)
  
    }
  
  bigrams_to_keep <- c() 
  
  for (i in unique(na.omit(gram_series_for_hc$bigram))){
    
    bigram_count <- gram_series_for_hc %>% filter(bigram == i) %>% pull(bigram_count) %>% unique()
    trigram_count <-  pull_max_matched_ngram(gram_series_for_hc, trigram, trigram_count, i)
    tetragram_count <- pull_max_matched_ngram(gram_series_for_hc, tetragram, tetragram_count, i)
    pentagram_count <- pull_max_matched_ngram(gram_series_for_hc, pentagram, pentagram_count, i)
    
    if(bigram_count > (trigram_count * 1.8 ) & 
       bigram_count > (tetragram_count * 1.8 - 0.1) & 
       bigram_count > (pentagram_count * 1.8 - 0.2)  ){
      bigrams_to_keep <- c(bigrams_to_keep, i)
    }
  }
  
  trigrams_to_keep <- c() 
  for (i in unique(na.omit(gram_series_for_hc$trigram))){
    
    trigram_count <- gram_series_for_hc %>% filter(trigram == i) %>% pull(trigram_count) %>% unique()
    tetragram_count <- pull_max_matched_ngram(gram_series_for_hc, tetragram, tetragram_count, i)
    pentagram_count <- pull_max_matched_ngram(gram_series_for_hc, pentagram, pentagram_count, i)
    
    if(trigram_count > (tetragram_count * 1.8 - 0.3 ) & 
       trigram_count > (pentagram_count * 1.8 - 0.4 )  ){
      trigrams_to_keep <- c(trigrams_to_keep, i)
    }
  }
  
  # testfunction<- function(){
  tetragrams_to_keep <- c() 
  for (i in unique(na.omit(gram_series_for_hc$tetragram))){
    
    tetragram_count <- gram_series_for_hc %>% filter(tetragram == i) %>% pull(tetragram_count) %>% unique()
    pentagram_count <- pull_max_matched_ngram(gram_series_for_hc, pentagram, pentagram_count, i)
    
    if(tetragram_count > (pentagram_count * 1.8 - 0.5)  ){
      tetragrams_to_keep <- c(tetragrams_to_keep, i)
    }
  }
  # }
  
  out_df <-
  gram_series_for_hc %>% 
    filter(bigram %in% bigrams_to_keep | 
             trigram %in% trigrams_to_keep |
             tetragram %in% tetragrams_to_keep |
             !is.na(pentagram))
  
  return(out_df)
  
}

top_ngrams_plot(messages_df)

