library(tidyverse)
library(plotly)
library(shiny)

HW <-tibble(
  id = c("Lahav", "Ben", "Lia", "Haari", "Maya", "Oren", "Arie", "Lili", "Michal", "Or"),
  height = c(151, 174, 145, 186, 128, 136, 179, 163, 152, 134),
  weight = c(63, 81, 60, 91, 47, 57, 76, 72, 62, 52)
)

# make a range of intercepts (a) and slopes (b) for slider:

a <- seq(from = 47, to = 91, length.out =100)
b <- seq(from = -10, to = 10, length.out =100)

# Create sliders for intercept 'a' and slope 'b'
slider_a <- sliderInput("intercept", "Intercept (a):", min = -100, max = 100, value = 0, step = 1)
slider_b <- sliderInput("slope", "Slope (b):", min = -5, max = 5, value = 1, step = 0.1)


# make test plot with line as slider. 
tt <- 



p <- HW %>%
  ggplot(mapping = aes(x = height, y = weight)) +
  geom_point()

ggplotly(p)


dyn_func




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
  sliderInput("y_intercept", "Y-Intercept", min = -10, max = 10, step = 0.1, value = 0),
  sliderInput("slope", "Slope", min = 0, max = 1, step = 0.01, value = 1),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Create a data frame with x values
    x <- seq(0, 100, length.out = 10)
    df <- data.frame(x = x, y = input$y_intercept + input$slope * x)
    
    # Create a ggplot with a straight line
    p <- ggplot(df, aes(x, y)) +
      geom_line() +
      xlim(-10, 179) +
      ylim(-10, 91) +
      geom_point(data = HW, mapping = aes(x = height, y = weight)) +
      labs(x = "height", y = "weight")
    
    print(p)
  })
}

shinyApp(ui, server)





