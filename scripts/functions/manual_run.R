#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(pacman)
p_load(char = c("DataExplorer", 
                 "jsonlite",
                 "here",
                 "tidytext",
                 "gt",
                 "reactable",
                 "ggthemes", 
                 "highcharter",
                 "shiny", 
                 "shinydashboard",
                  "tokenizers",
                 "tidyverse",
                "zoo"
))


# use_github(protocol = "https", auth_token= Sys.getenv("GITHUB_PAT"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Source functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("scripts/functions.R"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Import DF ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~ Convert JSON file to a data frame ---- 
# messages_df <- 
#   "/Users/kenwosu/Downloads/WhatsApp Chat with Krisana Messerli.txt" %>% 
#   whatsapp_txt_to_clean_df()

messages_df <- 
  "/Users/kenwosu/Downloads/WhatsApp Chat with Esther Peev.txt" %>% 
  whatsapp_txt_to_clean_df()


options(highcharter.theme = hc_theme_hcrt())

newtheme <- hc_theme_merge(
  getOption("highcharter.theme"), 
  hc_theme(chart = list(backgroundColor = "transparent"), 
           colors = paletteer_d("ggsci::nrc_npg") %>% as.character() %>% str_sub(1,7))
)

options(highcharter.theme = newtheme)


messages_df %>% 
  complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                max(as.Date(timestamp)), 
                                by = "day")) %>% 
  mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
  mutate(period = as.Date(cut(.$timestamp, breaks = "month"))) %>% 
  group_by(period, sender) %>% 
  summarise(n = sum(counter)) %>% 
  filter(!is.na(sender)) %>% 
  arrange(period) %>% 
  hchart("areaspline", hcaes(x = period, y = n, group = sender, fill = sender), 
         alpha = 0.3
         ) %>% 
  hc_plotOptions(series = list(fillOpacity = 0.2)) %>% 
  hc_title(text= 'Number of messages sent in each month')



  messages_df %>%
  select(1:3, timestamp) %>% 
  complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                max(as.Date(timestamp)), 
                                by = "day")) %>% 
  # some "messages" dont have senders. e.g. someone called
  mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
  mutate(day = as.Date(cut(.$timestamp, breaks = "day"))) %>% 
  mutate(day_of_week = lubridate::wday(day,label = T)) %>% 
  group_by(sender, day_of_week) %>% 
  summarise(n = sum(counter)) %>% 
  ungroup() %>% 
  group_by(sender) %>% 
  complete(day_of_week) %>% 
  ungroup() %>% 
  mutate(n = replace_na(n, 0)) %>% 
  filter(!is.na(sender)) %>% 
  hchart("area", hcaes(x = day_of_week, y = n, group = sender, fill = sender)         )%>%
  hc_plotOptions(area = list(fillOpacity = 0.15)) %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "")) %>%
  hc_title(text= 'Messages per day of the week')


#~~  who sent more messages? ---- 

messages_df %>% 
  group_by(sender) %>% 
  summarise(count = n()) %>% 
  hchart("pie", hcaes(name = sender, y = count ), 
         innerSize = "40%", 
         showInLegend = TRUE, 
         dataLabels = list(enabled = FALSE)) %>% 
  hc_title(text= 'Total number of texts sent')
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ Top n words/grams  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

#~~ Common Words (minus stop) ---- 

top_words_to_list <-   . %>% 
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

series_for_hc <-  messages_df %>% top_words_to_list


  highchart() %>%
  hc_chart(type= 'bar')%>%
  hc_title(text= 'Words used most frequently')%>%
  hc_xAxis(list(categories=wordlist_for_hc, labels=list(step= 1)))%>%
  hc_plotOptions(series=list(stacking= 'normal'))%>%
  hc_series(
    list(name= series_for_hc[[1]][1] %>% flatten_chr(),
         data= series_for_hc[[1]][2] %>% flatten_dbl()),
    
    list(name= series_for_hc[[2]][1] %>% flatten_chr(),
         data= series_for_hc[[2]][2]  %>% flatten_dbl()))
  
  
#~~ Common bigrams (minus stop) ---- 

#top_bigrams_minus_stop_to_list <- . %>%

top_bigrams_minus_stop <-   messages_df %>%   
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
  group_by(gram) %>% mutate(n_total = sum(n)) %>% ungroup()
  
  #~~ common trigrams (including stop) ---- 
top_trigrams_with_stop <-  
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
  
gram_series_for_hc <- 
  bind_rows(top_bigrams_minus_stop, top_trigrams_with_stop) %>% 
  arrange(-n_total) %>% 
  top_n_sample_ties(rank_column = n_total, nb_to_samp = 60) %T>% 
  {unique(.$gram) ->> trigram_wordlist_for_hc} %>% 
  select(-n_total) %>% 
  group_by(sender) %>%  
  do(data = .$n) %>% 
  list_parse() 
  
testdf <- bind_rows(top_bigrams_minus_stop, top_trigrams_with_stop) 


highchart() %>%
  hc_chart(type= 'bar')%>%
  hc_title(text= 'Phrases used most frequently')%>%
  hc_xAxis(list(categories=trigram_wordlist_for_hc, labels=list(step= 1)))%>%
  hc_plotOptions(series=list(stacking= 'normal'))%>%
  hc_series(
    list(name= trigram_series_for_hc[[1]][1] %>% flatten_chr(),
         data= trigram_series_for_hc[[1]][2] %>% flatten_dbl()),
    
    list(name= trigram_series_for_hc[[2]][1] %>% flatten_chr(),
         data= trigram_series_for_hc[[2]][2]  %>% flatten_dbl()))


#~~ common words (excluding stop) ----

word_series_for_hc <-
  messages_df %>%
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
  {
    expand(., sender, word) ->> permut 
  } %>%
  right_join(permut) %>%
  mutate(n = replace_na(n, 0)) %>%
  group_by(sender) %>% arrange(word, sender) %>% ungroup() %>%
  # fill in missings in "n_total" column.
  group_by(word) %>% mutate(n_total = first(na.omit(n_total))) %>% ungroup() %>%
  # then take the top most common grams (grouped so you don't catch an n-gram for only one person)
  pivot_wider(id_cols = c(word, n_total), values_from = n, names_from = sender) %>%
  top_n_sample_ties(rank_column = n_total, nb_to_samp = 20) %>%
  arrange(-n_total)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Time plots ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


messages_df %>% 
  complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                max(as.Date(timestamp)), 
                                by = "day")) %>% 
  mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
  mutate(period = as.Date(cut(.$timestamp, breaks = "month"))) %>% 
  group_by(period) %>% 
  summarise(n = sum(counter)) %>% 
  arrange(period) %>% 
  hchart("areaspline", hcaes(x = period, y = n), color = "pink") 



ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose JSON File", accept = ".json")
      ),
    mainPanel(
      reactableOutput("contents")
    )
  )
)
# 
file <- list()
file$datapath <- here("data/message_1.json")

server <- function(input, output) {
  
  json_to_df <- reactive(){
    file <- input$file1
    req(file)
    # read the JSON file into a data frame
    jsonlite::fromJSON(txt = file$datapath) %>% 
      .["messages"] %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      rename(sender = 1, timestamp_ms = 2, content = 3 ) %>% 
      mutate(content  = iconv(content, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
      filter(!str_detect(content, "sent an attachment")) %>% 
      filter(!str_detect(content, "video chat ended")) %>% 
      mutate(timestamp = ms_to_date(timestamp_ms))
    
  }
  
  
  output$table1 <- renderReactable({reactable(json_to_df())})
  
  
}

shinyApp(ui, server)


