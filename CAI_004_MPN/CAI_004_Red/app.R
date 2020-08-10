#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("magrittr")
library("nnet")

readRDS("CAI_004_Red/neural_netw.rds") -> neural_netw

preguntas <- c("1. ¿Con frecuencia siento frío en los pies en la cama?", "2. Cuando subo escaleras, ¿frecuentemente subo de dos en dos escalones?", "3. ¿Creo que jugaría bien en un equipo de baloncesto?", "4. Como oficial de policía, ¿yo impresionaría mucho?", "5. ¿Me siento incómodo en la mayoría de los carros?", "6. Literalmente, ¿yo miro a mis colegas de arriba hacia abajo?", "7. ¿Soy capaz de coger un objeto sobre un armario sin una escalera?", "8. ¿Tengo que agacharme al pasar por una puerta?", "9. ¿Soy capaz de colocar el equipaje en el maletero de un avión?", "10. Usualmente, ¿debo correr la silla del carro hacia atrás?", "11. Cuando subo al carro de otra persona, ¿me ofrecen la silla de adelante?", "12. Al tomar una fotografía, ¿me piden que me coloque atrás?", "13. Tengo dificultad para acomodarme en un bus?", "14. En una fila por orden de estatura, ¿siempre estoy atrás?", "15. Cuando tengo que escoger entre pasar por arriba o abajo, ¿escojo por arriba?", "16. Cuando estoy en una piscina, ¿yo toco el suelo con la planta de los pies?", "17. ¿Soy capaz de cambiar un bombillo sin subirme en algún objeto?", "18. Usualmente, ¿tengo que esquivar objetos colgantes?", "19.  Usualmente cuando estoy sentado, ¿mis pies quedan en el aire?")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Red Neuronal Artificial para la Estatura"),
  
  checkboxGroupInput(
    inputId = "preg",
    label = "Preguntas",
    choices = preguntas,
    selected = NULL,
    width = "90%"
  ),

  hr(),

  textOutput("result")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$result <- renderText({
    input$preg %>% unlist -> p
    (preguntas %in% p) %>% lapply(as.numeric) %>% 
      as.list %>% data.frame %>% 
      setNames(paste0("P", c(1:19))) -> new_data
      predict(neural_netw, new_data) 
    # names(new_data)
    # ggplotly(tsne_plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

