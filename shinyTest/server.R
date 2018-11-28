norm = function(x, y, mu, sigma) {
  x = matrix(c(x, y), 1, 2)
  k = 1 / sqrt((2 * pi) ^ 2 * det(sigma))
  e = exp(-0.5 * (x - mu) %*% solve(sigma) %*% t(x - mu))
  return(k * e)
}

server = function(input, output) {
  
  output$plot = renderPlot({
    #Создаем тестовые данные
    par(bg = 'black', fg = 'white')

    sigma = matrix(c(input$Ex,0,0,input$Ey), nrow = 2, ncol = 2 ) 
    mu = matrix(c(input$Mx, input$My), 1, 2)
    print(mu)
    m1 = (max(abs(input$Mx),abs(input$My)))
    m2 = (max(abs(input$Ex),abs(input$Ey)))
    m = max(m1,m2)
    q=m+4;
    s=q/50;
    plot(-q:q, -q:q, type = "n",asp=1,xlab="Ось x", ylab="Ось y", col.lab="orange",col.axis="orange")
    x=seq(-q, q, s)
    y=seq(-q, q, s)
    
    for(i in x){
      for(j in y){
        z = c(i, j)
        plot = norm(i,j,mu,sigma)
        color = adjustcolor("red", plot*(m2/5)*10 )
        points(z[1], z[2], pch = 21,col=color,bg=color)
      }
    }

    z = outer(x, y, function(x, y) {
      sapply(1:length(x), function(i) norm(x[i], y[i], mu, sigma))
      
    })
    
    contour(x,y,z,add=T ,asp=1,lwd = 1, col = "white")
  })
}







