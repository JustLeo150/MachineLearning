euclideanDistance <- function(u, v)
{
  sqrt(sum(u - v)^2)
}

weightsKWNN = function(i, k)
{
   (k + 1 - i) / k
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

kwnn <- function(xl, z, k)
{

  orderedXl <- sortObjectByDist(xl, z)
  n <- dim(orderedXl)[2] - 1

 
  weigths = rep(0,k)
  classes <- orderedXl[1:k, n+1]
  
  for(i in 1:k)
  {
    
    weigths[i]<-weightsKWNN(i,k);
  }
  x <- cbind(classes, weigths)


   counts <- table(x)

  class <- names(which.max(counts))
  return (class)
}


colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
X<-sample(c(1:150),15,replace=TRUE)
xl <- iris[X, 3:5]
plot(iris[X, 3:4], pch = 21, bg = colors[xl$Species], col = colors[xl$Species],asp=1)
u<-c(4,2)
classU<-kwnn(xl,u,k=5)


points(u[1],u[2],pch=21,bg=colors[as.numeric(classU)])






