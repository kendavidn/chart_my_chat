library(shinydashboard)
library(shinydashboardPlus)
library(shiny)


shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      minified = TRUE, 
      collapsed = TRUE,
      sidebarMenu(
        id = "sidebarMenu",
        lapply(1:3, function(i) {
          menuItem(
            sprintf("Menu %s", i), 
            tabName = sprintf("menu_%s", i), 
            icon = icon("circle")
          )
        })
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "menu_1", "Content 1"), 
        tabItem(
          tabName = "menu_2",
          box(
            title = "Always the same plot!",
            collapsible = TRUE, 
            plotOutput("distPlot")
          )
        )
      )
    ),
    #controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output, session) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    # Switch controlbar menu based on sidebar item value. Moreover
    # if the sidebar menu item is 2, the controlbar opens
    observeEvent(input$sidebarMenu, {
      idx <- strsplit(input$sidebarMenu, "_")[[1]][2]
      if (idx == 2) {
        updateControlbar("controlbar")
      }
      updateControlbarMenu("controlbarMenu", selected = idx)
    })
    
    # Clicking on the second controlbar item makes the box sidebar open
    observeEvent(input$controlbarMenu, {
      if (input$controlbarMenu == "Tab 2") updateBoxSidebar("boxSidebar")
    })
    
    observeEvent(input$num, {
      updateSliderInput(session, "obs", value = input$num)
    }, ignoreInit = TRUE)
    
  }
)
    