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
                 
      makeJoinedRow("Количество", "Col", 1, 5000, 2500), 
      makeJoinedRow("Смещение по X", "Ex", -5, 5, 0),
      makeJoinedRow("Смещение по Y", "Ey", -5, 5, 0),
      makeJoinedRow("дисперсия  X", "Mx", 1, 10, 10),
      makeJoinedRow("дисперсия Y ", "My", 1, 10, 10),
      
      
      h3("Класс Синие"),
      
      makeJoinedRow("Количество", "Col2", 1, 5000, 2500), 
      makeJoinedRow("Смещение по X", "Ex2", -5, 5, 0),
      makeJoinedRow("Смещение по Y", "Ey2", -5, 5, 0),
      makeJoinedRow("дисперсия  X", "Mx2", 1, 10, 1),
      makeJoinedRow("дисперсия Y", "My2", 1, 10, 1)
    ),
    
    mainPanel(
      
      plotOutput(outputId = "plot", height = "600px"),
      textOutput("msg"), style = "color: red; text-align: center; font-size:48px")
  )
  
  
)

