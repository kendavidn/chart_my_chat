
top_words_barplot <- function(messages_df, max_nb = 20) {
  
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
    top_n_sample_ties(rank_column = n_total, nb_to_samp = max_nb) %>%
    # drop people who have no utterances in the final list
    select_if( ~ !is.numeric(.) || sum(.) != 0 ) %>% 
    arrange(-n_total)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~  Create highcharts plot with possible multiple senders  ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # create vector of people in the chat
  senders <- names(word_series_for_hc)[-c(1,2)]
  
  # initialize list
  out_text <- list()
  
  # function that pastes the number of data lists necessary for plot
  # final list is different since it won't have a separating comma
  make_highchart_list_multiple_senders <- function(senders){
    for (i in 3:(length(senders)+2)  ){
      out_text[i] <- paste0("list(name= names(word_series_for_hc)[", i, "] ,", 
                            " data= word_series_for_hc[", i, "] %>% pull()),")
    }
    
    for (i in (length(senders)+2)  ){
      out_text[i] <- paste0("list(name= names(word_series_for_hc)[", i, "] ,", 
                            " data= word_series_for_hc[", i, "] %>% pull())")
    }
    
    return(out_text %>% 
             flatten_chr() %>% 
             paste(collapse = " "))
  }
  
  final_expr_for_plot <- paste("highchart() %>%
    hc_chart(type= 'bar') %>%
    hc_title(text= 'Words used most frequently')%>%
    hc_xAxis(list(categories=word_series_for_hc[1] %>% pull() , labels=list(step= 1)))%>%
    hc_plotOptions(series=list(stacking= 'normal'))%>%
    hc_series(", make_highchart_list_multiple_senders(senders), ")")
  
  plot <- eval(str2expression(final_expr_for_plot))
  
  return(plot)
  
}

