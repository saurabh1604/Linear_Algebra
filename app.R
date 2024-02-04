# Load required libraries
library(shiny)
library(plotly)

# UI definition
ui <- fluidPage(
  titlePanel("Linear Transformation Visualization App"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("a", label = "Matrix Element a", value = 1),
      numericInput("b", label = "Matrix Element b", value = 0),
      numericInput("c", label = "Matrix Element c", value = 0),
      numericInput("d", label = "Matrix Element d", value = 1),
      numericInput("input_x", label = "Input Vector X", value = 3),
      numericInput("input_y", label = "Input Vector Y", value = 4),
      sliderInput("transformation_progress", label = "Transformation Progress", min = 0, max = 1, value = 0.5, step = 0.1)
    ),
    
    mainPanel(
      plotlyOutput("linear_transformation_plot"),
      uiOutput("transformation_details")
    )
  )
)

# Server logic
server <- function(input, output) {
  # Example: Visualizing linear transformation on a 2D plane
  output$linear_transformation_plot <- renderPlotly({
    # Generate a grid of points
    x_range <- seq(-10, 10, length.out = 50)
    y_range <- seq(-10, 10, length.out = 50)
    
    # Create a grid using expand.grid
    grid <- expand.grid(x = x_range, y = y_range)
    
    # Convert grid to a matrix
    grid_matrix <- as.matrix(grid)
    
    # Define the matrix for linear transformation
    transformation_matrix <- matrix(c(as.numeric(input$a), as.numeric(input$b), as.numeric(input$c), as.numeric(input$d)), nrow = 2)
    
    # Apply linear transformation to each point in the grid
    transformed_grid <- as.data.frame((grid_matrix * (1 - input$transformation_progress)) + (as.matrix(grid_matrix %*% t(transformation_matrix)) * input$transformation_progress))
    colnames(transformed_grid) <- c("x", "y")
    
    # Get input vector coordinates
    input_vector <- data.frame(x = as.numeric(input$input_x), y = as.numeric(input$input_y))
    
    # Apply linear transformation to the input vector
    transformed_input_vector <- as.data.frame((input_vector * (1 - input$transformation_progress)) + (as.matrix(input_vector %*% t(transformation_matrix)) * input$transformation_progress))
    colnames(transformed_input_vector) <- c("x", "y")
    
    # Create vectors from origin to the input and its transformed point
    vectors_input <- data.frame(
      x = c(0, input_vector$x),
      y = c(0, input_vector$y)
    )
    
    vectors_output <- data.frame(
      x = c(0, transformed_input_vector$x),
      y = c(0, transformed_input_vector$y)
    )
    
    # Create a plotly scatterplot for visualization
    p <- plot_ly() %>%
      add_trace(type = "scatter", mode = "markers", x = ~grid$x, y = ~grid$y, name = "Original Grid", marker = list(color = "lightgray")) %>%
      add_trace(type = "scatter", mode = "markers", x = ~transformed_grid$x, y = ~transformed_grid$y, name = "Transformed Grid", marker = list(color = "darkgray")) %>%
      add_trace(type = "scatter", mode = "lines", x = vectors_input$x, y = vectors_input$y, name = "Input Vector", line = list(color = "blue", width = 2)) %>%
      add_trace(type = "scatter", mode = "lines", x = vectors_output$x, y = vectors_output$y, name = "Output Vector", line = list(color = "red", width = 2)) %>%
      layout(
        title = "Linear Transformation Visualization",
        xaxis = list(title = "X-axis"),
        yaxis = list(title = "Y-axis")
      )
    
    p
  })
  
  # Output matrix and vector details
  output$transformation_details <- renderUI({
    matrix_output <- matrix(c(as.numeric(input$a), as.numeric(input$b), as.numeric(input$c), as.numeric(input$d)), nrow = 2, byrow = TRUE)
    matrix_html <- as.character(matrix_output)
    
    details <- paste(
      "<strong style='color: blue;'>Matrix:</strong> [",
      matrix_html[1], ", ", matrix_html[2], "; ",
      matrix_html[3], ", ", matrix_html[4], "]",
      "<br>",
      "<strong style='color: green;'>Input Vector:</strong> [", input$input_x, ", ", input$input_y, "]",
      "<br>",
      "<strong style='color: red;'>Output Vector:</strong> [", round(transformed_input_vector$x, 2), ", ", round(transformed_input_vector$y, 2), "]"
    )
    
    HTML(details)
  })
}

# Run the application
shinyApp(ui, server)
