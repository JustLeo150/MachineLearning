euclideanDistance = function(u, v)
{
  return (sqrt(sum((u - v)^2)))
}
kernelG = function(r){
  return (((2*pi)^(-1/2)) * exp(-1/2*r^2))
}

Parzen = function(XL,y,h,ForMap)
{
  
  n = dim(XL)[1]
  weights = rep(0,3)
  names(weights) = c("setosa", "versicolor", "virginica")
  for(i in 1:n)
  {
    x=XL[i,1:2]
    class=XL[i,3]
    
    
    r = euclideanDistance(x,y)/h
    weights[class]=kernelG(r)+weights[class]
  }
  class = names(which.max(weights))

  if(max(weights)==0){
    if(ForMap==TRUE)
      return("silver") 
    else
      return(0)
  }
  else{
    if(ForMap==TRUE)
      return(class) 
    else
     return(max(weights))
    
  }
}


margin = function(points,classes,point,class){

  Myclass = points[which(classes==class), ]
  OtherClass = points[which(classes!=class), ]
  
  MyMargin = Parzen(Myclass,point[1:2],1,FALSE)
  OtherMargin = Parzen(OtherClass,point[1:2],1,FALSE)
  
  return(MyMargin-OtherMargin)
  
}


stolp = function(points, classes,errors) {
  n = length(classes)
  margins = rep(0, n)
  for (i in 1:n){
    margins[i] = margin(points, classes, points[i,], classes[i])
  }

  
  #plot(1:n, sort(margins), col="black", bg="blue",pch=20,
  #     main = "Отступы для Парзеновского окна(Гауссовского)",
  #     ylab = "Отступ ", xlab = "Данные", col.lab = "red")
  #lines(1:n, sort(margins), lwd = 2, col = "black")
  #lines(c(1, n), c(0, 0), col = "grey", lwd = 2)
  

  badpoints = which(margins < 0)
  print(length(badpoints))
  pointsWE=points[-badpoints,]
  classes = classes[-badpoints]
  n = n - length(badpoints)
  etalone = data.frame()
  
  for (class in unique(classes)) {
    print(class)
    ind = which(class == classes )
    margins = sapply(ind, function(i) margin(pointsWE, classes, pointsWE[i,], class))
    maxMarg = ind[which.max(margins)]
    etalone=rbind(etalone, pointsWE[maxMarg,])
    pointsWE=pointsWE[-maxMarg,]
    classes=classes[-maxMarg]
    n=n-1
  }
  names(etalone) = names(points)
  # print(etalone)
  # print(etaloneClasses)
  # print(pointsWE)
  # print(classes)
  while(n!=length(etalone)){
    count=0
    margins = c()
    index = c()
    for(i in 1:n)
    {
      m = margin(etalone, etalone[,3], pointsWE[i,], classes[i])
      if(m<=0){
        count=count+1;
        margins = c(margins, m)
        index = c(index,i)
      }
    }
    
   
    print(count)
    if( count < errors )
    {
      plot(pointsWE[,1:2],col = colors[classes], pch = 21, asp = 1, main = "STOLP для парзеновского окна(Гауссовского)")
      points(etalone[,1:2], bg = colors[etalone[,3]], pch = 21)
      print(etalone)
      plot(etalone[,1:2],bg = colors[etalone[,3]], pch = 21, asp = 1, main = "Карта Классификации STOLP")
      
      for(i in seq(0, 7, 0.1)){
        for(j in seq(0,3,0.1)){
          z = c(i, j)
          class = Parzen(etalone,z,1,TRUE)
           points(z[1], z[2], pch = 1,col=colors[class])
        }
      }
      points(pointsWE[,1:2],col = colors[classes], bg = colors[classes], pch = 21)
    
     break;
    }
    
   # plot(pointsWE[,1:2],col = colors[classes], pch = 21, asp = 1, main = "STOLP для парзеновского окна(Гауссовского)")
   # points(etalone[,1:2], bg = colors[etalone[,3]], pch = 21)
   # print(etalone)
    MinMarg = index[which.min(margins)]
    etalone = rbind(etalone, pointsWE[MinMarg,])
    pointsWE = pointsWE[-MinMarg,]
    classes = classes[-MinMarg]
    n = n - 1
    
  }
}



par(mfrow = c(1, 2))
colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue","grey"="grey")
xl = iris[, 3:5] 
classes = iris[, 5]
stolp(xl,classes,3)



