euclideanDistance = function(u, v)
{
  return (sqrt(sum((u - v)^2)))
}
kernelG = function(r){
  return (((2*pi)^(-1/2)) * exp(-1/2*r^2))
}

Parzen = function(XL,y,h)
{
  
  n = dim(XL)[1]
  weights = rep(0,3)
  names(weights) = c("setosa", "versicolor", "virginica")
  for(i in 1:n)
  {
    x=XL[i,1:2]
    class=XL[i,3]
    
    r = euclideanDistance(x,y)
 
    weights[class]=kernelR(r)+weights[class]
    
    
  }

  class = names(which.max(weights))

  if(max(weights)==0){
    return (0)
  }
  else{
    return (max(weights))
  }
}


margin = function(points,classes,point,class){

  Myclass = points[which(classes==class), ]
  OtherClass = points[which(classes!=class), ]
  
  MyMargin = Parzen(Myclass,point[1:2],0.1)
  OtherMargin = Parzen(OtherClass,point[1:2],0.1)
  
  return(MyMargin-OtherMargin)
  
}




stolp = function(points, classes,errors) {
  n = length(classes)
  margins = rep(0, n)
  for (i in 1:n){
    margins[i] = margin(points, classes, points[i,], classes[i])
  }
  badpoints = which(margins < -3)
  pointsWE=points[-badpoints,]
  classes = classes[-badpoints]
  n = n - length(badpoints)
  etalone = data.frame()
  etaloneClasses = c()

  
  
  for (class in unique(classes)) {
    print(class)
    ind = which(class == classes )
    margins = sapply(ind, function(i) margin(pointsWE, classes, pointsWE[i,], class))
    maxMarg = ind[which.max(margins)]
    etalone=rbind(etalone, pointsWE[maxMarg,])
    etaloneClasses=c(etaloneClasses,class)
    pointsWE=pointsWE[-maxMarg,]
    classes=classes[-maxMarg]
    n=n-1
  }
  # print(etalone)
  # print(etaloneClasses)
  # print(pointsWE)
  # print(classes)
 
  while(n!=length(etalone)){
    count=0
    margins = c()
    for(i in 1:n)
    {
      margins[i] = margin(etalone, etaloneClasses, pointsWE[i,], classes[i])
      if(margins[i]<=0){
        count=count+1;
      }
    }
    
   
    print(count)
    if( count < errors )
    {
      plot(pointsWE[,1:2],col = colors[classes], pch = 21, asp = 1, main = "STOLP для парзеновского окна(Гауссовского)")
      points(etalone[,1:2], bg = colors[etaloneClasses], pch = 21)
      
      plot(etalone[,1:2],bg = colors[etaloneClasses], pch = 21, asp = 1, main = "Карта Классификации STOLP")
      for(i in seq(0, 7, 0.1)){
        for(j in seq(0,3,0.1)){
          z = c(i, j)
          class = Parzen(etalone,z,0.4)
            points(z[1], z[2], pch = 1,col=colors[class])
         
        }
      }
      break;
    }
    
    MinMarg = which.min(margins)
    print(min(margins))
    etalone = rbind(etalone, pointsWE[MinMarg,])
    print(classes[MinMarg])
    etaloneClasses=c(etaloneClasses,classes[MinMarg])
    pointsWE = pointsWE[-MinMarg,]
    classes = classes[-MinMarg]
    n = n - 1
  }
 

}



par(mfrow = c(1, 2))
colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue", "NA" = "NA")
xl = iris[, 3:5] 
classes = iris[, 5]
stolp(xl,classes,3)




