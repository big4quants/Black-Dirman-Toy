## Refer to the "String model" for construction of term structure

sigma <- read_excel("C:/Users/mulin/Desktop/MFE Spring 2019/Fixed Income/HW7/Homework 7 sigma.xlsx", col_names = FALSE)
sigma = as.matrix(sigma)
sigma = sigma[,1]

# Forward-Swap-Rate function
FSR <- function(T1,T2){
  fsr <- (pfilea[2*T1]-pfilea[2*(T1+T2)])/sum(pfilea[(2*T1+1):(2*(T1+T2))])*2
  return(fsr)
}

swaption = function(T1,T2,Dt){
  fsr = FSR(T1,T2) # forward swap rate as per defined above
  
  set.seed(0)
  # use string model to simulate Dt up until T1
  Dt3 <- matrix(ncol = 11,nrow = 20)
  Dt3[,1] <- Dt[1:20]
  Dt3[cbind(1:20,2:11)] = 1
  pay_off = c(0)
  for(i in 1:10000){
    for(t in 2:(2*T1+1)){
      r=2*(1/Dt3[t-1,t-1]-1)
      normal = matrix(rnorm(20),ncol=1)
      dw = corchol%*%normal * sqrt(0.5)
      Dt3[t:20,t] = Dt3[t:20,t-1]+r*Dt3[t:20,t-1]/2+sigma[1:(21-t)]*Dt3[t:20,t-1]*dw[t:20]
    }
    # the current value of swap at time T1 is 1 as per defined
    pay_off = pay_off + pmax(fsr/2 * sum(Dt3[(2*T1+1):(2*(T1+T2)),(2*T1+1)]) + Dt3[(2*(T1+T2)),(2*T1+1)]- 1, 0)
  }
  price = pay_off/10000 * pfilea[2*T1] # take mean and discount
  return(price)
}
