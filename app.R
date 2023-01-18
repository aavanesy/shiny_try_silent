library(shiny)
library(dplyr)
library(ggplot2)

# separate function for the plot
fun_plot <- function(x = 12){
  data.frame(var1 = month.abb[1:x],
             var2 = 1:x) %>% 
    mutate(var1 = factor(var1, levels = month.abb)) %>% 
    ggplot(aes(x = var1, y = var2)) +
    geom_point()
}

ui <- shiny::fluidPage(
  sliderInput('input_x', label = 'x', min = -5, value = 4, max = 12),
  uiOutput('error'), #output variable for errors
  plotOutput('plot1')
)

server <- function(input, output) {
  
  error_reactive <- reactiveVal('')
  
  output$error <- renderUI(error_reactive())
  
  output$plot1 <- renderPlot({
    # we assume that the function is imperfect or the widgets are incorrect
    p1 <- try(fun_plot(x = input$input_x), silent = T)
    if(length(class(p1)) == 1 && class(p1) == 'try-error'){
      error_reactive(p1 %>% as.character()) #replace the '' by error message
      p1
    }else{
      error_reactive('')
      p1 # all good, render plot
    }
  })
}

shinyApp(ui = ui, server = server)