# Load the package required to read JSON files.
library(pacman)
p_load(char = c( "tidyverse",
                 "DataExplorer", 
                 "jsonlite",
                 "here",
                 "tidytext",
                 "gt",
                 "ggthemes", 
                 "highcharter",
))

# change to undo

use_github(protocol = "https", auth_token= Sys.getenv("GITHUB_PAT"))

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
  x %>%
    mutate(n_rand = {{ rank_column }} + sample(size = n(), x = (1:1000) / 10000, replace = T)) %>%
    top_n(25, wt = n_rand) %>%
    select(-n_rand)
}


#~~ milliseconds to date function  ---------------------------

ms_to_date <-  function(ms, t0="1970-01-01", timezone = "America/New_York" ) {
  sec <-  ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Import DF ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~ Convert JSON file to a data frame ---- 
messages_df <- 
  jsonlite::fromJSON(txt = here("data/messages_emem.json") ) %>% 
  .["messages"] %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rename(sender = 1, timestamp_ms = 2, content = 3 ) %>% 
  mutate(content  = iconv(content, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  filter(!str_detect(content, "sent an attachment")) %>% 
  filter(!str_detect(content, "video chat ended")) %>% 
  mutate(timestamp = ms_to_date(timestamp_ms))


#~~  who sent more messages? ---- 

messages_df %>% 
  group_by(sender) %>% 
  summarise(count = n())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ Top n words/grams  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~ Common Words (minus stop) ---- 

common_words_no_stop <- 
  messages_df %>%
  split(.$sender) %>%
  map(.f = function(df) {
    df$content %>%
      paste(collapse = " ") %>%
      data_frame() %>%
      rename(text = 1) %>%
      unnest_tokens(output = word, input = text) %>%
      anti_join(tidytext::stop_words) %>%
      count(word, sort = T) %>%
      top_n_sample_ties() 
  }) %>% 
  bind_rows(.id = "sender") %>% 
  mutate(sender = fct_rev(sender)) %>% 
  group_by(word) %>% mutate(n_total = sum(n)) %>% ungroup() %>% 
  mutate(word = fct_reorder(word, n_total)) # %>% 


highchart() %>% 
  hc_add_series(common_words_no_stop, type = "bar", hcaes(x = word, y = n, group = sender), 
                dataSorting = list(enabled =T)) %>% 
  hc_xAxis(categories = common_words_no_stop$word)



common_words_no_stop %>% 
  ggplot() + 
  geom_bar(aes(x = word, y = n, group = sender, fill = sender ), stat = "identity", position = "dodge") + 
  coord_flip()


#~~ common bigrams (minus stop) ---- 
messages_df$content %>% 
  paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
  unnest_tokens(output = gram, input = text, token = "ngrams", n = 2) %>% 
  separate(gram, c("word1", "word2"), sep = " ") %>% 
  filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
  transmute(gram = paste(word1, word2)) %>% 
  count(gram, sort = T) %>% 
  top_n_sample_ties() %>% 
  gt_tabulate_rank()

#~~ common trigrams (minus stop) ---- 
messages_df$content %>% 
  paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
  unnest_tokens(output = gram, input = text, token = "ngrams", n = 3) %>% 
  separate(gram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(across( .fns = ~ !(.x %in% tidytext::stop_words$word) )) %>% 
  transmute(gram = paste(word1, word2, word3)) %>% 
  count(gram, sort = T) %>% 
  top_n_sample_ties() %>% 
  gt_tabulate_rank()

#~~ common trigrams (including stop) ---- 
messages_df$content %>% 
  paste(collapse = ' ') %>% data_frame() %>% rename(text = 1) %>% 
  unnest_tokens(output = gram, input = text, token = "ngrams", n = 3) %>% 
  count(gram, sort = T) %>% 
  top_n_sample_ties() %>% 
  gt_tabulate_rank()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Time plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


timeplot_df <- messages_df %>% 
  complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                max(as.Date(timestamp)), 
                                by = "day")) %>% 
  mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
  mutate(period = as.Date(cut(.$timestamp, breaks = "month"))) %>% 
  group_by(period) %>% 
  summarise(n = sum(counter)) %>% 
  arrange(period)


highchart() %>% 
  hc_add_series(timeplot_df, "areaspline", hcaes(x = period, y = n), color = "pink")



#create dataframe pokemon
pokemon <- pokemon <- structure(list(type_1 = c("poison", "poison", "poison", "poison", 
                                                "rock", "rock", "rock", "rock", "rock", "rock", "rock", "rock", 
                                                "rock", "poison", "rock", "rock", "rock", "rock", "rock", "rock", 
                                                "rock", "rock", "rock", "rock", "rock", "poison", "poison", "poison", 
                                                "poison", "poison", "poison", "rock", "rock", "rock", "rock", 
                                                "rock", "rock", "poison", "poison", "rock", "rock", "rock", "rock", 
                                                "rock")
                                     , type_2 = c("ground", "ground", "flying", "flying", "ground", 
                                                  "ground", "ground", "ground", "water", "water", "water", "water", 
                                                  "flying", "flying", "ground", "ground", "dark", "psychic", "psychic", 
                                                  "grass", "grass", "bug", "bug", "steel", "steel", "dark", "dark", 
                                                  "bug", "dark", "fighting", "fighting", "steel", "flying", "flying", 
                                                  "fighting", "water", "water", "water", "dragon", "dragon", "dragon", 
                                                  "ice", "ice", "fairy"))
                                , class = c("tbl_df", "tbl", "data.frame"
                                )
                                , row.names = c(NA, -44L))

# create chart
pokemon <- pokemon %>% 
  group_by(type_1, type_2) %>% 
  summarise(n = n())%>%
  arrange(type_2)

# create chart
highchart() %>% 
  hc_add_series(pokemon, type = "bar", hcaes(x = type_2, group = type_1, y = n), 
                dataSorting = list(enabled =T)
                
  ) %>% 
  hc_xAxis(categories = pokemon$type_2) %>% 
  hc_plotOptions(series = list(stacking = "normal"))




