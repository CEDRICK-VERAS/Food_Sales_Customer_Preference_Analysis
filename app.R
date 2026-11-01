
library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
food_sales <- read.csv("food_sales.csv")

# Calculate revenue
food_sales$Revenue <- food_sales$Quantity *
  food_sales$UnitPrice

# Dashboard interface
ui <- fluidPage(
  
  titlePanel(
    "Food Sales and Customer Preference Analysis"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "category",
        "Select Food Category:",
        choices = c(
          "All Categories",
          sort(unique(food_sales$Category))
        )
      ),
      
      selectInput(
        "payment",
        "Select Payment Method:",
        choices = c(
          "All Methods",
          sort(unique(food_sales$PaymentMethod))
        )
      )
    ),
    
    mainPanel(
      
      h3("Sales Overview"),
      
      textOutput("totalRevenue"),
      textOutput("totalItems"),
      
      h3("Sales by Product"),
      
      plotOutput("productChart"),
      
      h3("Revenue by Category"),
      
      plotOutput("categoryChart"),
      
      h3("Daily Revenue Trend"),
      
      plotOutput("dailyChart")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    
    data <- food_sales
    
    if (input$category != "All Categories") {
      data <- data %>%
        filter(Category == input$category)
    }
    
    if (input$payment != "All Methods") {
      data <- data %>%
        filter(PaymentMethod == input$payment)
    }
    
    data
  })
  
  output$totalRevenue <- renderText({
    
    paste(
      "Total Revenue: PHP",
      format(
        sum(filtered_data()$Revenue),
        big.mark = ","
      )
    )
  })
  
  output$totalItems <- renderText({
    
    paste(
      "Total Items Sold:",
      sum(filtered_data()$Quantity)
    )
  })
  
  output$productChart <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(Product) %>%
      summarise(
        Quantity = sum(Quantity),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = reorder(Product, Quantity),
        y = Quantity,
        fill = Product
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Food Product",
        y = "Quantity Sold"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$categoryChart <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(Category) %>%
      summarise(
        Revenue = sum(Revenue),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(
        x = Category,
        y = Revenue,
        fill = Category
      )
    ) +
      geom_col() +
      labs(
        x = "Food Category",
        y = "Revenue (PHP)"
      ) +
      theme_minimal()
  })
  
  output$dailyChart <- renderPlot({
    
    data <- filtered_data() %>%
      group_by(Date) %>%
      summarise(
        Revenue = sum(Revenue),
        .groups = "drop"
      )
    
    ggplot(
      data,
      aes(x = Date, y = Revenue)
    ) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "darkorange", size = 2) +
      labs(
        x = "Date",
        y = "Revenue (PHP)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
