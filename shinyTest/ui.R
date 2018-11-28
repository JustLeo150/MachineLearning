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
  titlePanel("Линии уровня"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      makeJoinedRow("дисперсия  X", "Ex", 1, 10, 1),
      
      makeJoinedRow("дисперсия  Y", "Ey", 1, 10, 1),
      
      makeJoinedRow("Смещение по X", "Mx", -5, 5, 0),
      
      makeJoinedRow("Смещение по Y", "My", -5, 5, 0)
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "plot", height = "600px")
      
    )
    
  )
  
)