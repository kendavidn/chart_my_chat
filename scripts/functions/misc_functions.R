
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

