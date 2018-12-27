nameWidth = 3
inputWidth = 9

makeSlider = function(id, min, max, value) {
  sliderInput(
    inputId = id,
    label = NULL,
    min = min,
    max = max,
    value = value
  )
}

makeCountRow = function(id, min, max, value) {
  fluidRow(
    column(nameWidth, h5("Количество")),
    column(inputWidth, makeSlider(id, min, max, value))
  )
}

makeCovMatrixRow = function(id, min, max, values) {
  fluidRow(
    column(nameWidth, h5("Дисперсия")),
    column(inputWidth,
           fluidRow(
             column(6, makeSlider(paste0(id, "X"), min, max, values[1])),
             column(6, makeSlider(paste0(id, "Y"), min, max, values[2]))
           )
    )
  )
}

makeExpectedRow = function(id, min, max, values) {
  fluidRow(
    column(nameWidth, h5("Центр X,Y")),
    column(inputWidth,
           fluidRow(
             column(6, makeSlider(paste0(id, "X"), min, max, values[1])),
             column(6, makeSlider(paste0(id, "Y"), min, max, values[2]))
           )
    )
  )
}

makeColoredText = function(text, color) {
  h5(text, style = paste0("text-align: center; color: ", color))
}

makeColoredOutput = function(id, color) {
  span(textOutput(id), style = paste0("text-align:center; color: ", color))
}

ui <- fluidPage(
  

  titlePanel("Линейные алгоритмы"),


fluidRow( column(6,
                fluidRow( 
          column(2,checkboxInput("A",  makeColoredText("Adaline", "brown"), TRUE)),
          
          column(2,checkboxInput("P", makeColoredText("perseptron", "green"),FALSE)),
          
          column(2,checkboxInput("R", makeColoredText("regression", "orange"), FALSE))),
          column(12),
  column(nameWidth, h4("Количество итераций")),
  column(12),
         fluidRow(
           column(2, makeColoredText("Adaline", "brown")),
           column(2, makeColoredText("perseptron", "green")),
           column(2, makeColoredText("regression", "orange")),
           column(12),
           column(12),
           column(2, makeColoredOutput("adaline", "brown")),
           column(2, makeColoredOutput("perseptron", "green")),
           column(2, makeColoredOutput("regression", "orange"))
         )
  )

),

  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        column(nameWidth),
        column(inputWidth, h2("Класс Красные", style = "color: red; text-align: center; margin-top: 0"))
      ),
      
      makeCountRow("n1", 10, 200, 50),
      makeCovMatrixRow("cov1", 1, 20, c(5, 5)),
      makeExpectedRow("mu1", -50, 50, c(-50, 0)),
      
      fluidRow(
        column(nameWidth),
        column(inputWidth, h2("Класс Синие", style = "color: blue; text-align: center; margin-top: 0"))
      ),
      
      makeCountRow("n2", 10, 200, 50),
      makeCovMatrixRow("cov2", 1, 20, c(5, 5)),
      makeExpectedRow("mu2", -50, 50, c(50, 0))

      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "plot", height = "600px")
      
    )
    
  )
  

)