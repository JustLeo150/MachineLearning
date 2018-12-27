library(MASS)
library("gamlss")
# Нормализация обучающей выборки
normalize = function(xl) {
  cols = dim(xl)[2]
  
  for (i in 1:cols) {
    xl[, i] = (xl[, i] - min(xl[, i])) / (max(xl[, i]) - min(xl[, i]))
  }
  return(xl)
}

# Добавляем w0 = -1 как третью колонку
prepare = function(xl) {
  rows = dim(xl)[1]
  cols = dim(xl)[2]
  xl = cbind(xl, rep(-1, rows))
}


# Функции потерь
adaline = function(x) {
  (x - 1) ^ 2
}

perceptron = function(x) {
  max(-x, 0)
}

regression = function(x) {
  log2(1 + exp(-x))
}


adalineW = function(w, eta, xi, yi) {
  w - eta * (sum(w * xi) - yi) * xi
}

perceptronW = function(w, eta, xi, yi) {
  w + eta * yi * xi
}

regressionW = function(w, eta, xi, yi) {
  sigmoid = function(z) {
    1 / (1 + exp(-z))
  }
  
  w + eta * xi * yi * sigmoid(-sum(w * xi) * yi)
}

server = function(input, output) {
  
  # Стохастический градиент
  stoh = function(xl, classes, L, updateRule, iterId) {
    #изначальная настройка алгоритма
    rows = dim(xl)[1]
    cols = dim(xl)[2]
  
    w = runif(cols, -1 / (2 * cols), 1 / (2 * cols))
  
    lambda = 1 / rows
    
    # начальное Q
    Q = 0
    for (i in 1:rows) {
      margin = sum(w * xl[i,]) * classes[i]
      Q = Q + L(margin)
    }
    Q.prev = Q
    
    iter = 0
    repeat {
      iter = iter + 1
      
      # выбрать объекты с ошибкой
      margins = rep(0, rows)
      for (i in 1:rows) {
        xi = xl[i,]
        yi = classes[i]
        margins[i] = sum(w * xi) * yi
      }
      errorIndecies = which(margins <= 0)
     
      #выходим, если выборки полностью разделены
      if ((length(errorIndecies) == 0) & iterId=="perseptron" ) {
        break;
      }
     
      # выбираем случайный ошибочный объект
    
      if(length(errorIndecies)!=0)
      i = sample(errorIndecies, 1)
      else
      i = sample(1:rows,1)
      
      xi = xl[i,]
      yi = classes[i]
      
      # высчитываем ошибку
      margin = sum(w * xi) * yi
      error = L(margin)
      
      # обновляем веса
      eta = 1 / iter
      w = updateRule(w, eta, xi, yi)
      
      # новое Q
      Q = (1 - lambda) * Q + lambda * error
      
      # выходим, если Q стабилизировалось
      if (abs(Q.prev - Q) / abs(max(Q.prev, Q)) < 1e-5)
        break;
      
      # выходим, если слишком много итераций
      if (iter == 20000)
        break;
      
      Q.prev = Q
    }
 
    # выводим количество итераций
    output[[iterId]] = renderText({
      paste0(iter)
    })
    
    return(w)
  }
  
  drawLine = function(w, color) {
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 2, col = color)
  }
  
  output$plot = renderPlot({
    #Создаем тестовые данные
    output[["adaline"]] = renderText({
      paste0()
    })
    output[["perseptron"]] = renderText({
      paste0()
    })
    output[["regression"]] = renderText({
      paste0()
    })

   
    
    x.data <- rNO(input$n1,input$mu1X,input$cov1X)
   
    y.data <- rNO(input$n1,input$mu1Y,input$cov1Y)
   
    
    x2.data <- rNO(input$n2,input$mu2X,input$cov2X)
    y2.data <- rNO(input$n2,input$mu2Y,input$cov2Y)
    
    
    x1 <- cbind(x.data,y.data)
    x2 <- cbind(x2.data,y2.data)
    
    
    y1 = rep(-1, input$n1)
    y2 = rep(+1, input$n2)
    
    xl = rbind(x1, x2)
    classes = c(y1, y2)
    

    
    
    # Нормализация данных
     xl = normalize(xl)
     xl = prepare(xl)
    
     xl1 = xl[classes == -1,]
     xl2 = xl[classes == 1,]
     xl = rbind(xl1, xl2)
     colors = c(rep("red", dim(xl1)[1]), rep("blue", dim(xl2)[1]))
     
     plot(xl[, ], pch = 21, bg = colors, col="black",asp = 1, xlab = "X", ylab = "Y")
     if(input$A | input$P | input$R)
       legend("bottomleft", legend=c(if(input$A)"adaline", if(input$P)"perseptron", if(input$R)"regression"),
              col=c(if(input$A)"brown", if(input$P)"green", if(input$R)"orange"), lty=1, cex=1.5)

    # Поиск разделяющей поверхности
    if(input$A==TRUE)
    w1 = stoh(xl, classes, adaline, adalineW, iterId = "adaline")
    
    if(input$P==TRUE)
    w2 = stoh(xl, classes, perceptron, perceptronW, iterId = "perseptron")
    if(input$R==TRUE)
    w3 = stoh(xl, classes, regression, regressionW, iterId = "regression")
    
    # Рисуем разделяющую поверхность
    if(input$A==TRUE)
    drawLine(w1, "brown")
    if(input$P==TRUE)
    drawLine(w2, "green")
    if(input$R==TRUE)
    drawLine(w3, "orange")
  })
}