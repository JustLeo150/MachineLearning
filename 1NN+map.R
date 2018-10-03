euclideanDistance <- function(u, v)
{
  sqrt(sum(u - v)^2)
}
sortObjectByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- rep(0, l)
  for (i in 1:l)
  {
    distances[i] <- c(metricFunction(xl[i, 1:n], z))
   
  }
  orderedXl <- xl[order(distances), ]
  return (orderedXl)
}

ONN<-function(xl, z)
{
  orderedXl<- sortObjectByDist(xl, z)
  n<-dim(orderedXl)[2]
  classes <- orderedXl[1, n]
  return (classes)
}


  colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
 X<-sample(c(1:150),15,replace=TRUE)
 xl <- iris[X, 3:5]
 plot(iris[X, 3:4], pch = 21, bg = colors[xl$Species], col = colors[xl$Species],asp=1)


 
for(i in seq(0, 7, 0.07)){
  for(j in seq(0, 3, 0.1))
  {
    z <- c(i, j)
    class <- ONN(xl, z)
    points(z[1], z[2], pch = 1, bg = colors[class],col=colors[class])
  }
}
