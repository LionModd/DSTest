library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
 

ui <- dashboardPage(
  dashboardHeader(title = "Mock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Volcanos", tabName = "volcano"),
      menuItem("mpg", tabName = "mpg"))),
  dashboardBody(
    
  tabItems(
    # Volcanos dashboard
    tabItem(tabName = "volcano",
            fluidRow(plotOutput("plot1", click = "plot.click" ,
                      hover = hoverOpts(id = "plot.hover", delay = 200),
                      brush = brushOpts(id = "plot_brush")),
            fluidRow(verbatimTextOutput("summary")),
            fluidRow(
                 box(title = "click", verbatimTextOutput("info")),
                 box(title = "hover", verbatimTextOutput("hover")), 
                 box(title = "brush", verbatimTextOutput("brush")),
                 box(title = "brush summary", verbatimTextOutput("brushsummary"))))),
    
    # mtCars dashboard
    tabItem(tabName = "mpg",
            fluidRow(box(title = "mpg", plotOutput("plot2", brush = "brush")),
                     box(title = "Linked Brush", plotOutput("linkedbrush"))))
    
  
  ))
)


server <- function(input, output){
  output$plot1 <- renderPlot({
    plot(faithful)
  })
  
  output$plot2 <- renderPlot({
    ggplot(mtcars) +
      geom_point(aes(x = mpg, y = hp))
  })
  
  output$linkedbrush <- renderPlot({
    brushed <- brushedPoints(mtcars, input$brush)
    
    ggplot(mtcars, aes(x = wt, mpg)) + 
      geom_point() +
      geom_point(data = brushed, colour = "red", size = 3)
  })
  
  output$summary <- renderPrint({
    summary(faithful)
  })
  
  output$info <- renderText({
    paste0("eruptions=", input$plot.click$x, "\n",
           "waiting=", input$plot.click$y)})
    
  output$hover <- renderText({
    paste0("eruptions=", input$plot.hover$x, "\n",
           "waiting=", input$plot.hover$y, "\n")
  })
  
  output$brush <- renderPrint({
    rows <- brushedPoints(df = faithful,
                          brush = input$plot_brush,
                          xvar = "eruptions",
                          yvar = "waiting")
    cat("Brushed points: \n")
    print(rows)
  })
  
  
  output$brushsummary <- renderPrint({
    summary(brushedPoints(df = faithful,
                          brush = input$plot_brush,
                          xvar = "eruptions",
                          yvar = "waiting"))
  })
}

shinyApp(ui = ui, server = server)

