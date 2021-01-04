
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~ tabulate function --------------------------- 
gt_tabulate_rank <- 
  . %T>% 
  {max(.["n"]) ->> max_count } %>%
  gt() %>% 
  tab_options(table.font.size = "60%") %>% 
  data_color(
    columns = "n",
    colors = scales::col_numeric(
      palette = c( "#ebf7eb" , "green4"),
      domain = range(1:max_count)))


#~~ top_n function (sample ties) --------------------------- 

top_n_sample_ties <- function(x, rank_column = n, nb_to_samp = 25) {
  set.seed(1)
  x %>%
    mutate(n_rand = {{ rank_column }} + sample(size = n(), x = (1:1000) / 10000, replace = T)) %>%
    top_n(nb_to_samp, wt = n_rand) %>%
    select(-n_rand)
}


#~~ milliseconds to date function  ---------------------------

ms_to_date <-  function(ms, t0="1970-01-01", timezone = "America/New_York" ) {
  sec <-  ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}


#~~ milliseconds to date function  ---------------------------


top_words_rank_for_hc <-   . %>%                                                  
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = " ") %>%
      data_frame() %>%
      rename(text = 1) %>%
      unnest_tokens(output = word, input = text) %>%
      anti_join(tidytext::stop_words) %>%
      count(word, sort = T) # %>%
    #      top_n_sample_ties(nb_to_samp = 20) 
  }) %>% 
  bind_rows(.id = "sender") %>% 
  group_by(word) %>% mutate(n_total = sum(n)) %>% ungroup() %T>% 
  {expand(., sender, word) ->> permut #var in pipechain
  } %>% 
  right_join(permut) %>% 
  mutate(n = replace_na(n, 0)) %>% 
  group_by(sender) %>% 
  arrange(word, sender) %>% 
  ungroup() %>% 
  mutate(n_total =zoo::na.locf(n_total)) %>% 
  arrange(-n_total) %>% 
  top_n_sample_ties(rank_column = n_total, nb_to_samp = 30) %T>% 
  {unique(.$word) ->> wordlist_for_hc #var in pipechain
  } %>% 
  select(-n_total) %>% 
  top_words_rank_for_hc() %>% 
  group_by(sender) %>%  
  do(data = .$n) %>% 
  list_parse() 








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
    
    if(bigram_count > trigram_count * 1.8  & 
       bigram_count > tetragram_count * (1.8 - 0.1) & 
       bigram_count > pentagram_count * (1.8 - 0.2)  ){
      bigrams_to_keep <- c(bigrams_to_keep, i)
    }
  }
  
  trigrams_to_keep <- c() 
  for (i in unique(na.omit(gram_series_for_hc$trigram))){
    
    trigram_count <- gram_series_for_hc %>% filter(trigram == i) %>% pull(trigram_count) %>% unique()
    tetragram_count <- pull_max_matched_ngram(gram_series_for_hc, tetragram, tetragram_count, i)
    pentagram_count <- pull_max_matched_ngram(gram_series_for_hc, pentagram, pentagram_count, i)
    
    if(trigram_count > tetragram_count * (1.8 - 0.3 ) & 
       trigram_count > pentagram_count * (1.8 - 0.4 )  ){
      trigrams_to_keep <- c(trigrams_to_keep, i)
    }
  }
  
  # testfunction<- function(){
  tetragrams_to_keep <- c() 
  for (i in unique(na.omit(gram_series_for_hc$tetragram))){
    
    tetragram_count <- gram_series_for_hc %>% filter(tetragram == i) %>% pull(tetragram_count) %>% unique()
    pentagram_count <- pull_max_matched_ngram(gram_series_for_hc, pentagram, pentagram_count, i)
    
    if(tetragram_count > pentagram_count * (1.8 - 0.5)  ){
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