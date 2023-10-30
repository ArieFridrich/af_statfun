library(tidyverse)
library(plotly)
library(shiny)

HW <-tibble(
  id = c("Lahav", "Ben", "Lia", "Haari", "Maya", "Oren", "Arie", "Lili", "Michal", "Or"),
  height = c(18, 8, 5, 10, 3, 20, 16, 11, 4, 17),
  weight = c(20, 6, 8, 12, 2, 14, 18, 10, 4, 16)
)


############3############3############3############3############3############3############3############3############
##############################################################################################################################


ui <- fluidPage(
  
  sliderInput("y_intercept", "Y-Intercept", min = 0, max = 20, step = 0.1, value = 10),
  sliderInput("slope", "Slope", min = -2, max = 2, step = 0.01, value = 0),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    # Create a data frame with x values
    x <- HW$height
    df <- data.frame(x_hw = x, y_hw = input$y_intercept + input$slope * x, data_y = HW$weight)
    
    # Create a ggplot with a straight line
    p <- ggplot(df, aes(x_hw, y_hw)) +
      geom_line(aes(color = "red")) +
      xlim(0, 20) +
      ylim(0, 25) +
      geom_point(data = HW, mapping = aes(x = height, y = weight)) +
      labs(x = "height", y = "weight") +
      geom_segment(mapping = aes(x = x_hw, y = data_y, xend = x_hw, yend = y_hw))
    print(p)
  })
}

shinyApp(ui, server)

