makeJoinedRow = function(label, id, min, max, value) {
  fluidRow(
    column(2, h5(label)),
    column(10,
           sliderInput(
             inputId = id,
             label = NULL,
             min = min,
             max = max,
             value = value
           )
    )
  )
}

ui <- fluidPage(
  titlePanel("Подстановочный классификатор"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Класс Красные"),
      
      makeJoinedRow("Количество", "Col", 1, 1000, 500), 
      makeJoinedRow("Смещение по X", "Ex", -15, 15, -15),
      makeJoinedRow("Смещение по Y", "Ey", -15, 15, 0),
      
      
      
      h3("Общие параметры"),
      makeJoinedRow("дисперсия X", "Mx", 1, 10, 5),
      makeJoinedRow("дисперсия Y ", "My", 1, 10, 5),
      
      
      h3("Класс Синие"),
      
      makeJoinedRow("Количество", "Col2", 1, 1000, 500), 
      makeJoinedRow("Смещение по X", "Ex2", -15, 15, 15),
      makeJoinedRow("Смещение по Y", "Ey2", -15, 15, 0)
     
    ),
    
    mainPanel(
      
      plotOutput(outputId = "plot", height = "600px"),
      textOutput("msg"), style = "color: red; text-align: center; font-size:48px")
  )
  
  
)

