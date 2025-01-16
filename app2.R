# Load necessary libraries
library(shiny)
library(tidyverse)
library(rpart)       # For decision trees
library(rpart.plot)  # For plotting decision trees
library(caret)       # For confusion matrix

# Define UI
ui <- fluidPage(
  titlePanel("Decision Tree Classification"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "cleaned_data.csv", accept = ".csv"),
      uiOutput("independent_vars"),
      sliderInput("cp", "Complexity Parameter (cp):", min = 0, max = 0.1, value = 0.01, step = 0.01),
      actionButton("run_model", "Run Decision Tree")
    ),
    
    mainPanel(
      h3("Decision Tree Plot"),
      plotOutput("tree_plot"),
      h3("Decision Tree Rules"),
      verbatimTextOutput("tree_rules"),
      h3("Model Summary"),
      verbatimTextOutput("model_summary"),
      h3("Confusion Matrix"),
      verbatimTextOutput("confusion_matrix"),
      h3("Accuracy"),
      verbatimTextOutput("accuracy")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data and convert Result to 0 and 1
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df$Result <- ifelse(df$Result == 2, 1, 0) # Convert 2 to 1 and 1 to 0
    df
  })
  
  # Render UI for independent variable selection
  output$independent_vars <- renderUI({
    req(data())
    selectInput("independent_vars", "Select Independent Variables", 
                choices = names(data())[names(data()) != "Result"], 
                multiple = TRUE)
  })
  
  # Run decision tree model
  model <- eventReactive(input$run_model, {
    req(data(), input$independent_vars)
    formula <- as.formula(paste("Result ~", paste(input$independent_vars, collapse = "+")))
    rpart(formula, data = data(), method = "class", control = rpart.control(cp = input$cp))
  })
  
  # Plot decision tree
  output$tree_plot <- renderPlot({
    req(model())
    rpart.plot(model(), main = "Decision Tree", box.palette = "auto")
  })
  
  # Display decision tree rules
  output$tree_rules <- renderPrint({
    req(model())
    cat("Decision Tree Rules:\n")
    print(model(), digits = 2)
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  # Display confusion matrix
  output$confusion_matrix <- renderPrint({
    req(model(), data())
    pred <- predict(model(), data(), type = "class")
    confusionMatrix(pred, factor(data()$Result))
  })
  
  # Display accuracy
  output$accuracy <- renderText({
    req(model(), data())
    pred <- predict(model(), data(), type = "class")
    cm <- confusionMatrix(pred, factor(data()$Result))
    paste("Accuracy:", round(cm$overall["Accuracy"], 3))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)