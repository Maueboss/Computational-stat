
# X ~ pois(nu)
# 

X = rpois(100,lambda = 5)
mean(X)
var(X)
mean(X^3)
mean((X-mean(X))^3)

# I KNOW THE TRUTH!
# theta = cov(xbar, S2)
# 
n = 100

nsim = 5000
S = replicate(nsim,{
  x = rpois(n,lambda = 5)
  c(mean(x),var(x))
} )
S = t(S)
cov(S)
# truth
5/100
plot(S,pch="*")




THEsample = rpois(n,lambda = 5)
B = 2000
Boot_sample = replicate(B,{
  sample(x = THEsample,
         size = length(THEsample),
         replace = TRUE,
         prob = rep(1/length(THEsample),
                    length(THEsample)))
})


dim(Boot_sample)

boot_ests = t(apply(Boot_sample,2,
      function(a) c(mean(a),var(a))  ))

plot(boot_ests)
cov(boot_ests)





THEsample = rgamma(1000,5,2)
median(THEsample)

B = 10000
Boot_sample = replicate(B,{
  sample(x = THEsample,
         size = length(THEsample),
         replace = TRUE,
         prob = rep(1/length(THEsample),
                    length(THEsample)))
})

plot(density(THEsample))
for(l in 1:100){
lines(density(Boot_sample[,l]),
      col="darkred")
}
lines(density(THEsample),col="lightblue")

median(THEsample)

median_star = apply(Boot_sample,2,median)
hist(median_star)
abline(v= median(THEsample),lwd=2)


x = rgamma(100,6,2)

xbar = mean(x)

myownteststat = function(sample, theta0){
  abs(mean(sample) - theta0)
}

t_obs = myownteststat(x,3)
# H0 : a/b = 3 => a=3,b=1

boot_teststats = replicate(10000,{
          y = rgamma(length(x),3,1)
          myownteststat(y,3)
          })
boot_teststats
hist(boot_teststats)
abline(v = t_obs,lwd=4)

mean(boot_teststats>t_obs)





mtcars


THEmod = glm(vs~.,data = mtcars,
             family = "binomial")
THEbetas = THEmod$coefficients
THEbetas = THEmod$deviance

n = nrow(mtcars)
RES = replicate(5000,{
 
  ind = sample(1:n,size = n,replace = TRUE) 
  boot_cars = mtcars[ind,]
  bootmod = glm(vs~.,data = boot_cars,
               family = "binomial")
  c(bootmod$coefficients,
  bootmod$deviance)
  
})

RES = t(RES)
hist(RES[,2])
hist(RES[,12])



