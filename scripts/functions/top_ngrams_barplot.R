

top_ngrams_barplot <- function(messages_df, max_nb = 20) {
  
  # max_nb = 20
  
  count_grams <- function(df, slice_nb = 15, gram_no){ 
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
  
  bigrams <- messages_df %>% count_grams(gram_no = 2, slice_nb = 100)
  trigrams <- messages_df %>% count_grams(gram_no = 3, slice_nb = 80)
  tetragrams <- messages_df %>% count_grams(gram_no = 4, slice_nb = 60)
  pentagrams <- messages_df %>% count_grams(gram_no = 5,  slice_nb = 20)
  
  
gram_series_for_hc <- 
    bind_rows(bigrams, trigrams) %>% 
    bind_rows(tetragrams) %>% 
    bind_rows(pentagrams) %>% 
    # create new columns indicating what type of ngram each gram is
    mutate(bigram = ifelse(str_count(gram, '\\S+') == 2, gram, NA )) %>% 
    mutate(bigram_count = ifelse(!is.na(bigram), n, NA )) %>% 
    mutate(trigram = ifelse(str_count(gram, '\\S+') == 3, gram, NA )) %>% 
    mutate(trigram_count = ifelse(!is.na(trigram), n, NA )) %>% 
    mutate(tetragram = ifelse(str_count(gram, '\\S+') == 4, gram, NA )) %>% 
    mutate(tetragram_count = ifelse(!is.na(tetragram), n, NA )) %>% 
    mutate(pentagram = ifelse(str_count(gram, '\\S+') == 5, gram, NA )) %>% 
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~  Drop ngrams where all words are stopwords ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    separate(gram, into = c(paste0("word", 1:5)), sep = " ") %>% 
    mutate(word1stop = ifelse(word1 %in% stop_words$word, 1, 0), 
           word2stop = ifelse(word2 %in% stop_words$word, 1, 0), 
           word3stop = ifelse(word3 %in% stop_words$word, 1, 0), 
           word4stop = ifelse(word4 %in% stop_words$word, 1, 0), 
           word5stop = ifelse(word5 %in% stop_words$word, 1, 0)) %>% 
    rowwise() %>% 
    mutate(stop_sum = sum(word1stop, word2stop, word3stop, word4stop, word5stop)) %>% 
    ungroup() %>% 
    mutate(all_stop = NA, 
           all_stop = ifelse(!is.na(bigram) & stop_sum == 2, "yes. drop", all_stop ), 
           all_stop = ifelse(!is.na(trigram) & stop_sum == 3, "yes. drop", all_stop ), 
           all_stop = ifelse(!is.na(tetragram) & stop_sum == 4, "yes. drop", all_stop ), 
           all_stop = ifelse(!is.na(pentagram) & stop_sum == 5, "yes. drop", all_stop ), 
           all_stop = replace_na(all_stop, "not all stopwords. keep")) %>% 
    filter(all_stop == "not all stopwords. keep") %>% 
    unite(col = gram, word1, word2, word3, word4, word5, na.rm = TRUE, sep = " ") %>% 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~  Reshape ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    hc_legend(
        align= 'right',
        verticalAlign= 'top',
        layout= 'vertical',
        x= 0,
        y= 100) %>%
    hc_series(", make_highchart_list_multiple_senders(senders), ")")
  
  plot <- eval(str2expression(final_expr_for_plot))
  
  return(plot)
  
}


#top_ngrams_barplot(messages_df)

