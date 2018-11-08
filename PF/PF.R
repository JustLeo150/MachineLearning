euclideanDistance = function(u, v)
{
  return (sqrt(sum((u - v)^2)))
}
kernelEP = function(r){
  return ((3/4*(1-r^2)*(abs(r)<=1)))
}

PF = function(potentials,XL,y,h,metricFunction = euclideanDistance){
n = dim(XL)[1]
weights = rep(0,3)
names(weights) = c("setosa", "versicolor", "virginica")
for(i in 1:(n-1))
{
  x=XL[i,1:2]
  class=XL[i,3]
  
  r = metricFunction(x,y)/h
  
  weights[class]=potentials[i]*kernelEP(r)+weights[class]

}

class = names(which.max(weights))

if(max(weights)==0){

  return ("NA")
}
else{
  return (class)
}
}

potentials = function(XL,class,n,h,myError){
  error=100
  pots = rep(0,n)
  
  while(error>myError)
  {
    
    while(TRUE){
      z=sample(1:n,1)
      x=XL[z,1:2]
      point = PF(pots,XL,x,h)
   
      if (colors[point] != colors[class[z]]) {
        pots[z] = pots[z] + 1
        break
      }
      
    }
     
      error = 0
      for (i in 1:n) {
        x = XL[i,1:2]
        points=XL[,1:3]
        if (colors[PF(pots,points,x, h)]!= colors[class[i]]){
          error = error + 1
        }
      }
      print(error)
  }
  return(pots)
}


drawPotentials = function(XL, classes, potentials, h, colors) {
  plot(XL, bg = colors[classes], pch = 21, asp = 1,  main = "потенциалы")  
  transform  = potentials / max(potentials)
  for (i in 1:n) {
      x = XL[i, 1]
      y = XL[i, 2]
      color = adjustcolor(colors[classes[i]], transform[i] /3)
      draw.circle(x, y, h, 50, border = color, col = color)
    }
}



colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue", "NA" = "NA")
#X=sample(c(1:150),60,replace=TRUE) 
xl = iris[, 3:5] 
class = iris[, 5]
h = 1
myError = 6
n = dim(xl)[1]
Y = rep(0,n)
text = paste("Map classificaton for PW(EP) with h = ", h)
pot = potentials(xl,class,n,h,myError)
#print(pot)
drawPotentials(xl[,1:2], class, pot, h, colors)








