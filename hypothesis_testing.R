#Z-TEST
str(mtcars)
n <- length(mtcars$mpg)
a <- mean(mtcars$mpg)
sigma <- sd(mtcars$mpg)
mu <- 20

z <- sqrt(n) * (a - mu) / sigma
pvalue <- 2 * pnorm(-abs(z))
(z + pvalue)
?pnorm

#T_TEST

(t.test(mtcars$mpg, mu=10))
help(with)
#Test of normality(GOF)
with(mtcars, shapiro.test(mpg[am==0]))
with(mtcars, shapiro.test(mpg[am==1]))
#Test of group variance 
var.test(mpg ~ am, data = mtcars)
#using two sample t-test (Student)
t.test(mpg ~ am , var.equal = T , data = mtcars)
summary(lm(mpg ~ am , data = mtcars))
  