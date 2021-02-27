
# load libraries
library(ggplot2)
library(shiny)
library(dplyr)
library(gridExtra)


iris <- read.csv("Iris.csv")


iris_summary <- iris %>%
  group_by(Species) %>%
  summarise("Average sepal length" = mean(SepalLengthCm),
            "Average sepal width" = mean(SepalWidthCm),
            "Average petal length" = mean(PetalLengthCm),
            "Average petal length" = mean(PetalWidthCm))

ui <- fluidPage(

  titlePanel("ShinyApp"),
  
  
  sidebarLayout(position = "left",
    
    
    sidebarPanel(

      selectInput(
        inputId = "y",
        label = "y-axis",
        choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
        selected = "SepalLengthCm"
      ),
      
      
      
      # Select variable for x
      selectInput(
        inputId = "x",
        label = "x-axis",
        choices = c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm"),
        selected = "SepalWidthCm"
      ),
      
      
      
      # select species to be dynamically displayed in graph, by default all species are selected
      checkboxGroupInput(
        inputId = "species",
        label = "Select Species", 
        choices = c("Iris-setosa", "Iris-versicolor", "Iris-virginica"),
        selected = c("Iris-setosa", "Iris-versicolor", "Iris-virginica")
      )
    ),
    
    
    
    
    mainPanel(
      fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("scatterplot"), plotOutput("histogram"))),
      tags$h3("Data Table"), 
      tableOutput('table'),

      
    )
    
    
  )
)


server <- function(input, output) {
  
  df <- reactive({
    iris %>%
      filter(Species %in% input$species)
  })
 
  # table 
  output$table <- renderTable(iris_summary)
  
  # scatter plot 
  output$scatterplot <- renderPlot({
    ggplot(
      df(), 
      aes_string(x = input$x, y = input$y)) + 
      geom_point(aes(col = df()$Species), size=3) + scale_color_discrete(name ="Species") +
      geom_smooth(aes(group=df()$Species, color = df()$Species), method='lm')
    
  })
  
  output$histogram <- renderPlot({
    x <- iris$SepalLengthCm
    hist(x)
    #ggplot(df(), aes(x=iris$SepalLengthCm)) + geom_histogram()
    #x    <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    #hist(x, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
    
  })
  
  
}

# combine ui and server in a shinyApp
shinyApp(ui = ui, server = server)