
library(pacman)
p_load(
    "shiny",
    "shinydashboard",
    "highcharter",
    "shinyWidgets",
    "gapminder",
    "tidyverse"
)


ui <- dashboardPage(
    dashboardHeader(
        title = "Country Trends for the Top 40 Countries",
        titleWidth = "95vh",
        # replacement for dropdown menu
        tags$li(
            class = "dropdown",
            tags$li(HTML("<a href='https://www.w3schools.com/' target= '_blank' >About</a>")
            )
        )
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            column(
                width = 10,
                pickerInput(
                    inputId = "selected_country",
                    label = "select countries",
                    selected = unique(gapminder$country)[1],
                    multiple = F,
                    choices = unique(gapminder$country) %>% as.character(),
                    width = "100%"
                )
            )
        ),
        fluidRow(
            column(
                width = 12,
                column(width = 07, highchartOutput("linegraph1", height = "600px")),
                column(width = 05, highchartOutput("linegraph2", height = "600px"))
            )
        )
    )
)





server <- function(input, output) {
    output$linegraph1 <- renderHighchart({
        gapminder %>%
            filter(country == input$selected_country) %>%
            hchart("line", hcaes(x = year, y = lifeExp, group = country, color = country))
    })
    
    output$linegraph2 <- renderHighchart({
        gapminder %>%
            filter(country == input$selected_country) %>%
            hchart("line", hcaes(x = year, y = lifeExp, group = country, color = country))
    })
}


# Run the application
shinyApp(ui = ui, server = server)
