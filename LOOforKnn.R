## ????????? ??????????
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

## ????????? ??????? ???????? ?????????? ?? ??????? z
sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  ## ??????? ??????? ??????????
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  

  orderedXl <- xl[order(distances[, 2]), ]
  
  return (orderedXl);
}

knn <- function(xl, z, k)
{


  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  

  classes <- orderedXl[1:k, n + 1]
  


  counts <- table(classes)

  class <- names(which.max(counts))

  return (class)
}
## ?????? ???????
colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")





LOO = function(xl,class) {

  n = dim(xl)[1];
 
  loo = rep(0, n) 
  for(k in 1:n){
    for(i in 1:n){
      X=xl[-i, 1:3]
      u=xl[i, 1:2]
      test=knn(X,u,k)
    
      if(test!=class[i]){
        loo[k]=loo[k]+1;
       
      }
    } 
  }
  loo = loo / n
  x = 1:length(loo)
  y = loo
  plot(x, y,main ="LOO for KNN(k)", xlab="k", ylab="LOO", type = "l")
  
  min=which.min(loo)
  points(min, loo[min], pch = 21, col = "red",bg = "red")
}






X<-sample(c(1:150),30,replace=TRUE)
xl <- iris[X, 3:5] 
class <- iris[X, 5]
LOO(xl,class)



