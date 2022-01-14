#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(jsonlite)
library(here)
library(tidytext)
library(paletteer)
library(highcharter)
library(shiny)
library(shinyWidgets)
library(styler)
library(shinydashboard)
#
#library(shinydashboardPlus)
#
library(rwhatsapp)
library(tools)
library(waiter)
library(tidyverse)

options(highcharter.theme = hc_theme_hcrt())

newtheme <- hc_theme_merge(
    getOption("highcharter.theme"), 
    hc_theme(chart = list(backgroundColor = "transparent"), 
             colors = 
                 c("#332859",
                 (paletteer_d("ggsci::nrc_npg") %>% 
                 as.character() %>% 
                 str_sub(1,7))
                 ))
)
options(highcharter.theme = newtheme)

# hchart(cars, "scatter", hcaes(speed, dist))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~  Source functions ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("scripts/functions/misc_functions.R"))
source(here("scripts/functions/whatsapp_txt_to_clean_df.R"))
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


ui <-dashboardPage(
    header = dashboardHeader(disable = T, title = "Analyze your texts today at chartmychats.com", titleWidth = "95vh",
                    tags$li(class = "dropdown", tags$li(HTML("<a href='https://www.w3schools.com/' target= '_blank' >About</a>")))),
    sidebar = dashboardSidebar(disable = T),
    body = dashboardBody(
        use_waiter(), # include dependencies
        waiter_on_busy(),
        setBackgroundImage(
            src = "https://www.fonewalls.com/wp-content/uploads/Lavender-Gradient-Wallpaper.jpg", 
            shinydashboard = TRUE),
        fluidRow(
            column(width = 3, 
                   box(width = NULL,
                       div(style = 'overflow-x: scroll; ',
                           HTML("<h2>Prototype Shiny App to analyse FB and Whatsapp messages.</h2> <br>
                                Steps to export your messages:
                           <a href='https://tinyurl.com/exportwhatsappchat' target= '_blank' >Whatsapp</a>
                           or 
                           <a href='https://tinyurl.com/exportfacebookchat' target= '_blank' >Facebook</a>.<br>
                           Then upload a single conversation to the app.
                           Note that this app does NOT store any of your information. 
                           You can view all source code for the app on my <a href='https://github.com/kendavidn/chart_my_chat' target= '_blank' >github.</a>
                           Got other module ideas! I'm at kendavidn@gmail.com"))
                   )
            ),
            column(width = 6, 
                       #   class = "col-lg-offset-4 col-lg-4 col-md-offset-2 col-md-8 col-sm-offset-1 col-sm-10",
                          fileInput("file1", "Choose File", accept = c(".json", ".txt"), width = "100%")
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
        
        if (file_ext(file$datapath) == "txt"){
            whatsapp_txt_to_clean_df(whatsapp_txt = file$datapath)
            } else if (file_ext(file$datapath) == "json"){
            jsonlite::fromJSON(txt = file$datapath) %>% 
                json_list_to_clean_df()
            }
        })
    
    # analysis out
    output$message_count_donutplot <- renderHighchart(messages_df() %>% message_count_donutplot())
    
    output$word_count_donutplot <- renderHighchart(messages_df() %>% word_count_donutplot())
    
    output$avg_msg_length_barplot <- renderHighchart(messages_df() %>% avg_msg_length_barplot())
    
    output$msg_freq_over_time_lineplot <- renderHighchart(messages_df() %>% msg_freq_over_time_lineplot())
    
    output$msg_freq_weekly_radialplot <- renderHighchart(messages_df() %>% msg_freq_weekly_radialplot())
    
    output$top_words_barplot <- renderHighchart(messages_df() %>% top_words_barplot(max_nb = 30)) 
    
    output$top_ngrams_barplot <- renderHighchart(messages_df() %>% top_ngrams_barplot(max_nb = 30)) 
    
    
    
}


# Run the application
shinyApp(ui = ui, server = server)



