library("gamlss")


generateData <- function(input) {
  #rNO(количество значений в выборке, mu = среднее, sigma = стандартное отклонение)
  
  x.data <- rNO(input$Col,input$Ex,input$Mx)
  y.data <- rNO(input$Col,input$Ey,input$My)


  x2.data <- rNO(input$Col2,input$Ex2,input$Mx)
  y2.data <- rNO(input$Col2,input$Ey2,input$My)
  
  data <- cbind(x.data,y.data)
  data2 <- cbind(x2.data,y2.data)
  
  
  Mu1 <- assessmentMu(data)
  Mu2 <- assessmentMu(data2)

  color = adjustcolor("green", 10 )
  xl <- rbind(cbind(data,color),cbind(data2,"red"))
  plot(xl,col="black",pch=21,bg=xl[,3],asp=1,xlab="Ось x", ylab="Ось y", col.lab="orange",col.axis="orange")
  
  Sigma <-
    estimateFisherCovarianceMatrix(data,
                                   data2, Mu1, Mu2)
  ## Получаем коэффициенты ЛДФ
  inverseSigma <- solve(Sigma)
  alpha <- inverseSigma %*% t(Mu1 - Mu2)
  mu_st <- (Mu1 + Mu2) / 2
  beta <- mu_st %*% alpha
  ## Рисуем ЛДФ
  color = adjustcolor("black", 1 )
  abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col =
           color, lwd = 3)
  
}
assessmentMu <- function(xl) {
  n <- dim(xl)[1]
  m <- dim(xl)[2]
  Mu <- matrix(NA, 1, m)
  for(i in 1:m)
  {   
    Mu[1,i] <- mean(xl[, i])
  }
  return(Mu)
}

estimateFisherCovarianceMatrix <- function(objects1,
                                           objects2, mu1, mu2)
{
  rows1 <- dim(objects1)[1]
  rows2 <- dim(objects2)[1]
  rows <- rows1 + rows2
  cols <- dim(objects1)[2]
  
  sigma <- matrix(0, cols, cols)
  
  for (i in 1:rows1)
  {
    sigma <- sigma + (t(objects1[i,] - mu1) %*%
                        (objects1[i,] - mu1)) / (rows + 2)
  }
  
  for (i in 1:rows2)
  {
    sigma <- sigma + (t(objects2[i,] - mu2) %*%
                        (objects2[i,] - mu2)) / (rows + 2)
  }
  
  return (sigma)
}

server = function(input, output) {
  
  output$plot = renderPlot({
    xl <- data.frame
    vec <- generateData(input)
  })
}

