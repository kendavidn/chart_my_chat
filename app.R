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




options(highcharter.theme = hc_theme_elementary())

newtheme <- hc_theme_merge(
    getOption("highcharter.theme"), 
    hc_theme(chart = list(backgroundColor = "transparent"))
    )

options(highcharter.theme = newtheme)

# hchart(cars, "scatter", hcaes(speed, dist))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Source functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("scripts/functions/misc_functions.R"))
source(here("scripts/functions/json_list_to_clean_df.R"))


# donut plots
source(here("scripts/functions/word_count_donutplot.R"))
source(here("scripts/functions/message_count_donutplot.R"))

# message length
source(here("scripts/functions/avg_msg_length_barplot.R"))


# messages over time
source(here("scripts/functions/msg_freq_over_time_lineplot.R"))
source(here("scripts/functions/msg_freq_weekly_radialplot.R"))



# top words and phrases
source(here("scripts/functions/top_words_barplot.R"))
source(here("scripts/functions/top_ngrams_barplot.R"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~ ui ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ui <-
    dashboardPage(
        
    dashboardHeader(disable = TRUE, title = "Analyze your texts today at chartmychats.com", titleWidth = "95vh",
                    tags$li(class = "dropdown", tags$li(HTML("<a href='https://www.w3schools.com/' target= '_blank' >About</a>")))),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        setBackgroundImage(
            src = "https://www.fonewalls.com/wp-content/uploads/Lavender-Gradient-Wallpaper.jpg", 
            shinydashboard = TRUE),
        fluidRow(
            column(width = 3, 
                   box(width = NULL,
                       div(style = 'overflow-x: scroll; ',
                           "Tiny Shiny App to analyse Facebook messages. Still hoping to work on sentiment analysis, response time analysis, word networks (graphs), option to download as poster.")
                   )
            ),
            column(width = 6, 
                       #   class = "col-lg-offset-4 col-lg-4 col-md-offset-2 col-md-8 col-sm-offset-1 col-sm-10",
                          fileInput("file1", "Choose JSON File", accept = ".json", width = "100%")
                          )
                   ,
            column(width = 3, 
                          dropdownButton(tags$h4("Control Outputs"),
                                         "Will add options to subset by sender, dates",
                                         icon = icon("gear"), status = "info"), 
                          dropdownButton(tags$h4("Control Outputs"),
                                         "will add information about non-obvious text analysis decisions",
                                         icon = icon("info"), status = "info")
                          )
                   ),
        fluidRow(
            column(width = 12,
                   column(width = 4, highchartOutput("message_count_donutplot")), 
                   column(width = 4, highchartOutput("word_count_donutplot")),
                   column(width = 4, highchartOutput("avg_msg_length_barplot"))
                   )
            ),
        fluidRow(
            column(width = 8, highchartOutput("msg_freq_over_time_lineplot")),
            column(width = 4, highchartOutput("msg_freq_weekly_radialplot"))
            
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

    # data in 
    messages_df <- reactive({
        file <- input$file1 
        req(file)
        jsonlite::fromJSON(txt = file$datapath) %>% json_list_to_clean_df()
        })
    
    # analysis out
    output$message_count_donutplot <- renderHighchart(messages_df() %>% message_count_donutplot())
    
    output$word_count_donutplot <- renderHighchart(messages_df() %>% word_count_donutplot())
    
    output$avg_msg_length_barplot <- renderHighchart(messages_df() %>% avg_msg_length_barplot())
    
    output$msg_freq_over_time_lineplot <- renderHighchart(messages_df() %>% msg_freq_over_time_lineplot())
    
    output$msg_freq_weekly_radialplot <- renderHighchart(messages_df() %>% msg_freq_weekly_radialplot())
    
    output$top_words_barplot <- renderHighchart(messages_df() %>% top_words_barplot(max_nb = 20)) 
    
    output$top_ngrams_barplot <- renderHighchart(messages_df() %>% top_ngrams_barplot(max_nb = 20)) 
    
    
    
}


# Run the application
shinyApp(ui = ui, server = server)



