library(tidyverse)
library(plotly)
library(shiny)

HW <-tibble(
  id = c("Lahav", "Ben", "Lia", "Haari", "Maya", "Oren", "Arie", "Lili", "Michal", "Or"),
  height = c(151, 174, 145, 186, 128, 136, 179, 163, 152, 134),
  weight = c(63, 81, 60, 91, 47, 57, 76, 72, 62, 52)
)


############3############3############3############3############3############3############3############3############3

# Create a function to generate the data for the line
generate_line_data <- function(a, b) {
  x <- seq(0, 100, by = 1)
  y <- a + b*x
  data.frame(x = x, y = y)
}

# Create a ggplot2 plot of the HW dataset and the line data
p <- HW %>%
  ggplot(mapping = aes(x = height, y = weight)) +
  geom_point() +
  geom_line(
    data = generate_line_data(a = 50, b = 0.5),
    mapping = aes(x = x, y = y),
    color = "red"
  )

# Convert the ggplot2 plot to a ggplotly plot
ggplotly(p)

ggplotly(p) %>%
  add_sliders
  



##############################################################################################################################


ui <- fluidPage(
  sliderInput("y_intercept", "Y-Intercept", min = -20, max = 10, step = 0.1, value = 1),
  sliderInput("slope", "Slope", min = 0, max = 1, step = 0.01, value = 0.5),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Create a data frame with x values
    x <- HW$height
    df <- data.frame(x = x, y = input$y_intercept + input$slope * x)
    
    # Create a ggplot with a straight line
    p <- ggplot(df, aes(x, y)) +
      geom_point(aes(color = "red")) +
      xlim(100, 179) +
      ylim(30, 91) +
      geom_point(data = HW, mapping = aes(x = height, y = weight)) +
      labs(x = "height", y = "weight") +
      geom_segment(aes(x = 100, y = 80, xend = 130, yend = 70, color = "red"))
    print(p)
  })
}


shinyApp(ui, server)





