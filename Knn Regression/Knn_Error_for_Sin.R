library(caret)

set.seed(1)

n <- 100
x <- (1:n)/n
f <- sin(x)
y <- f + rnorm(n, mean = 0, sd= 1)

mse_knn <- function(X, Y, krange) {
  n <- length(Y)
  valset_error <- array(0,length(krange))
  
  for (i in 1:length(krange)) {
    K <- krange[i]
    fit <- knnreg(as.matrix(X), Y, K)
    
    pr <- predict(fit, c(1/2))
    
    valset_error[K] <- mean((1/2 - pr)^2) 
    
  }
  
  return (valset_error)
  
}

mse_knn(x,y,1:50)
error_mse <- mse_knn(x,y,1:50)

plot(1:50, error_mse ,xlab='k',ylab='error',main='Knn Error for K at x_0 = 1/2')
error_mse
selected_k = which.min(error_mse)
selected_k

lines(c(selected_k,selected_k), c(0,100), col= 'red',lwd=3)
