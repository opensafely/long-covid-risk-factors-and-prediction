X1 <- rnorm(10)
r1 <- rcs(X1, 3)

x1 <- c(30,40,32, 47, 44,50,60,70,90,65,45, 22)
r1 <-rcs(x1,3)
r1 = rcs(x1,4)

x1 = input$cov_num_age
r1 = rcs(x1,parms=knot_placement)
View(r1)
r1 <- matrix(r1)
splines <- r1$rms

a <- data.matrix(r1)
a1 <- a[,1]
a2 <- a[,2]

a <- data.frame(a1,a2)
