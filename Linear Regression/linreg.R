library(caret)
library(leaps)

#setting up the data file
dat <- read.csv("insurance.csv", header = TRUE)
attach(dat)

#assigning variables
Y <- dat$charges
X1 <- dat$bmi
X2 <- dat$age
partial_X3 <- dat$sex
desired_length <- length(partial_X3)
X3_list <-vector(mode = "list", length = desired_length)
counter <- 1
#assigning 1 to males, and 0 to females
for (sex in partial_X3) {
  if (sex == "male") {
    X3_list[counter] <- 1
    counter = counter + 1
  }
  else {
    X3_list[counter] <- 0
    counter = counter + 1
  }
}
X3 <- unlist(X3_list)
X4 <- dat$children

#creating a linear regression model
insurance_reg <- lm(Y ~ X1 + X2 + X3 + X4)
summary(insurance_reg)
plot(insurance_reg)

#best subset selection 
best_subset_models <- regsubsets(Y~ X1+X2+X3+X4, data = dat, nvmax = 4)
summary(best_subset_models)

#forward stepwise selection
step(insurance_reg, direction = "forward")
