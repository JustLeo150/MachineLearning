euclideanDistance <- function(u, v)
{

  return (sqrt(sum((u - v)^2)))

}

kernelEP = function(x,y,h,metricFunction = euclideanDistance){
 
   r = metricFunction(x,y)
    if(r<=h){
      return(3/4*(1-r^2))
    }

    return(0)
  
}



Parzen <- function(y,h)
{
  n = dim(xl)[1]
  weights = rep(0,3)
  names(weights) <- c("setosa", "versicolor", "virginica")
  for(i in 1:n)
  {
    x<-xl[i,1:2]
    class<-xl[i,3]
    weights[class]<-kernelEP(x,y,h)+weights[class];
  }

  class <- names(which.max(weights))
  if(max(weights)==0){
    return ("grey")
  }
  else{
    return (class)
  }
  

 

  

}

LOO = function(xl,class) {
  n = dim(xl)[1]
  loo = rep(0, 15)
  
  for(i in 1:(n)){

    u=xl[i, 1:2]

    for(h in 1:15){
      H <- h/5;
      test=Parzen(u,H)

      if(colors[test] != colors[class[i]]){
        loo[h] = loo[h]+1;
      }    
    } 
  }
 
  loo = loo / 15
  x = seq(0.2,3,0.2)
  y = loo
  plot(x, y,main ="LOO for PW(H)", xlab="h", ylab="LOO", type = "l")
  
  min=which.min(loo)
  lOOmin=round(loo[min],3)
  xmin=min*3
  minX=min/5
  points(minX, loo[min], pch = 21, col = "red",bg = "red")
  label = paste("   H = ", minX, "\n", "   LOO = ", lOOmin, sep = "")
  text(minX, lOOmin, labels = label, pos=4, col = "red")
  print("*******************************************************************")
  
  text <- paste("Map classificaton for PW with h = ", minX)
  plot(iris[X, 3:4],main=text, pch = 21, bg = colors[xl$Species], col = colors[xl$Species],asp='1')
  
  for(i in seq(0, 7, 0.1)){
    for(j in seq(0,3,0.1)){
      z <- c(i, j)

      class <- Parzen(z,minX)
   
      points(z[1], z[2], pch = 1, bg = colors[class],col=colors[class])
      
    }
  }

}


par(mfrow = c(1, 2))

colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue","grey"="grey")
X<-sample(c(1:150),60,replace=TRUE) 
xl <- iris[X, 3:5] 
class <- iris[X, 5]
LOO(xl,class)







