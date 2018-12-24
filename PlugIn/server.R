estimateMu <- function(objects)
{
  ## mu = 1 / m * sum_{i=1}^m(objects_i)
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  
  mu <- matrix(NA, 1, cols)
  for (col in 1:cols)
  {
    mu[1, col] = mean(objects[,col])
  }
  
  return(mu)
}
getPlugInDiskriminantCoeffs <- function(mu1, sigma1, mu2,sigma2)
{
  ## Line equation: a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2 + f = 0
  invSigma1 <- solve(sigma1)
  invSigma2 <- solve(sigma2)
  
  
  f <- log(abs(det(sigma1))) - log(abs(det(sigma2))) +
    mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*%
    t(mu2);
  
  alpha <- invSigma1 - invSigma2
  a <- alpha[1, 1]
  b <- 2 * alpha[1, 2]
  c <- alpha[2, 2]
  
  beta <- invSigma1 %*% t(mu1) - invSigma2 %*% t(mu2)
  d <- -2 * beta[1, 1]
  e <- -2 * beta[2, 1]
  return (c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y"
            = e, "1" = f))
}

generateData <- function(input) {
  #rNO(количество значений в выборке, mu = среднее, sigma = стандартное отклонение)
  x.data <- rNO(input$Col,input$Ex,input$Mx)
  y.data <- rNO(input$Col,input$Ey,input$My)
  help(data.frame)
  
  x2.data <- rNO(input$Col2,input$Ex2,input$Mx2)
  y2.data <- rNO(input$Col2,input$Ey2,input$My2)
  
  data <- cbind(x.data,y.data)
  data2 <- cbind(x2.data,y2.data)
  
  
  Mu1 <- assessmentMu(data)
  Mu2 <- assessmentMu(data2)
  Sigma1 <- assessmentSigma(Mu1,data)
  Sigma2 <- assessmentSigma(Mu2,data2)
  
  xl <- rbind(cbind(data,"red"),cbind(data2,"blue"))
  plot(xl,col="black",pch=21,bg=xl[,3],asp=1,xlab="Ось x", ylab="Ось y", col.lab="orange",col.axis="orange")
  
  coeffs <- getPlugInDiskriminantCoeffs(Mu1,Sigma1, Mu2,
                                        Sigma2)
  
  ## Рисуем дискриминантую функцию – черная линия
  
  x <- y <- seq(-10, 20, len=100)
  z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 +
               coeffs["xy"]*x*y
             + coeffs["y^2"]*y^2 + coeffs["x"]*x
             + coeffs["y"]*y + coeffs["1"])
  
  contour(x, y, z, levels=0, drawlabels=FALSE, lwd = 5, col =
            "black", add = TRUE)
  
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

assessmentSigma <- function(mu,xl){
  n = dim(xl)[1]
  m = dim(xl)[2]
  Sigma = matrix(0, m, m)
  for (i in 1:n) {
    Sigma = Sigma + (t(xl[i,] - mu) %*% (xl[i,] - mu)) / (n - 1)
  }
  return(Sigma)
}

server = function(input, output) {
  
  output$plot = renderPlot({
xl <- data.frame
vec <- generateData(input)
  })
}

