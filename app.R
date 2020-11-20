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
                "shinyWidgets",
                "styler",
                "shinydashboard",
                "tidyverse"
))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Source functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("scripts/functions/misc_functions.R"))
source(here("scripts/functions/top_words_plot.R"))
source(here("scripts/functions/top_ngrams_plot.R"))

dropdownButtonp <- purrr::partial(
    dropdownButton,
    status = "customstatus",
    size = "sm",
    right = TRUE,
    status = "info",
    width = "400px",
    inline = TRUE,
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ui <-
    dashboardPage(
        
    dashboardHeader(title = "Analyze your texts today at chartmychats.com", titleWidth = "95vh",
                    tags$li(class = "dropdown", tags$li(HTML("<a href='https://www.w3schools.com/' target= '_blank' >About</a>")))),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
          #  column(width = 12,
             #      class = "top-buffer",
                   column(width = 10, 
                          class = "col-lg-offset-4 col-lg-4 col-md-offset-2 col-md-8 col-sm-offset-1 col-sm-10",
                          fileInput("file1", "Choose JSON File", accept = ".json", width = "100%")
                          )
                   ,
                   column(width = 2, 
                          dropdownButtonp(tags$h4("Control Outputs"), 
                                          icon = icon("gear")), 
                          dropdownButtonp(tags$h4("Control Outputs"), 
                                          icon = icon("info"))
                          )
                   #)
            ),
        fluidRow(
            column(width = 12,
                   column(width = 4, highchartOutput("messages_sent_donutplot"))
                   )
            ),
        fluidRow(
            column(width = 12,
                   column(width = 9, highchartOutput("messages_over_time_lineplot"))
                   )
            ),
        fluidRow(
            column(width = 12,
                   column(width = 6, highchartOutput("top_words_barplot")),
                   column(width = 6, highchartOutput("top_ngrams_barplot"))
            )
        )
    )
)


server <- function(input, output) {

    
    messages_df <- reactive({
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
            filter(!str_detect(content, "missed a call from")) %>% 
            filter(!str_detect(content, "missed a video call from")) %>% 
            filter(!str_detect(content, "video chat ended")) %>% 
            mutate(timestamp = ms_to_date(timestamp_ms))
        })
    
    
    output$messages_sent_donutplot <- renderHighchart(
        
        messages_df() %>% 
            group_by(sender) %>% 
            summarise(count = n()) %>% 
            hchart("pie", hcaes(name = sender, y = count ), 
                   innerSize = "40%", 
                   showInLegend = TRUE, 
                   dataLabels = list(enabled = FALSE)) %>% 
            hc_title(text= 'Total number of texts sent')
        
        
        
    )
    
    output$messages_over_time_lineplot <- renderHighchart(
        messages_df() %>% 
        complete(timestamp = seq.Date(min(as.Date(timestamp)),
                                      max(as.Date(timestamp)), 
                                      by = "day")) %>% 
        mutate(counter = if_else(is.na(sender), 0, 1 )) %>% 
        mutate(period = as.Date(cut(.$timestamp, breaks = "month"))) %>% 
        group_by(period) %>% 
        summarise(n = sum(counter)) %>% 
        arrange(period) %>% 
        hchart("areaspline", hcaes(x = period, y = n), color = "pink") %>% 
        hc_title(text= 'Number of messages sent in each month')
            
            )
    
    
    output$top_words_barplot <- renderHighchart(top_words_plot(messages_df(), max_nb = 20)) 
    
    output$top_ngrams_barplot <- renderHighchart(top_ngrams_plot(messages_df(), max_nb = 20)) 
    
    
    
}


# Run the application
shinyApp(ui = ui, server = server)



