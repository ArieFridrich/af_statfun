library(tidyverse)
library(plotly)
library(shiny)

HW <-tibble(
  id = c("Lahav", "Ben", "Lia", "Haari", "Maya", "Oren", "Arie", "Lili", "Michal", "Or"),
  height = c(151, 174, 145, 186, 128, 136, 179, 163, 152, 134),
  weight = c(63, 81, 60, 91, 47, 57, 76, 72, 62, 52)
)


############3############3############3############3############3############3############3############3############
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
    df <- data.frame(x_hw = x, y_hw = input$y_intercept + input$slope * x, data_y = HW$weight)
    
    # Create a ggplot with a straight line
    p <- ggplot(df, aes(x_hw, y_hw)) +
      geom_line(aes(color = "red")) +
      xlim(100, 179) +
      ylim(30, 91) +
      geom_point(data = HW, mapping = aes(x = height, y = weight)) +
      labs(x = "height", y = "weight") +
      geom_segment(mapping = aes(x = x_hw, y = data_y, xend = x_hw, yend = y_hw))
    print(p)
  })
}



shinyApp(ui, server)





