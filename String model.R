library(readxl)
library(readr)
library(knitr)
library(zoo)
library(data.table)
pfilea <- read_excel("C:/Users/mulin/Desktop/MFE Spring 2019/Fixed Income/HW7/Homework 7 pfilea.xlsx", col_names = FALSE)
sigma <- read_excel("C:/Users/mulin/Desktop/MFE Spring 2019/Fixed Income/HW7/Homework 7 sigma.xlsx", col_names = FALSE)
corchol <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Fixed Income/HW7/Homework 7 corchol.csv", col_names = FALSE)
corrin <- read_csv("C:/Users/mulin/Desktop/MFE Spring 2019/Fixed Income/HW7/Homework 7 corrin.csv", col_names = FALSE)

sigma = as.matrix(sigma)
pfilea = as.matrix(pfilea)
corrin = as.matrix(corrin)
corchol = as.matrix(corchol)
sigma = c(0,sigma[,1])
sigma = matrix(sigma[1:20],ncol= 1)

# par rate 
CMS <- function(time){
  (1-pfilea[2*time])/sum(pfilea[1:(2*time)]) * 2
}

strike <- matrix(sapply(c(2:5,7,10),CMS),nrow=1) # par rate = strike rate
colnames(strike) = paste0(c(2:5,7,10),"-year")
rownames(strike) = "CMS"
kable(strike)
strike <- rbind(strike,c(2:5,7,10))
rownames(strike) <- c("CMS","year")

forwar_rate <-  (pfilea[1:25]/pfilea[2:26]-1) * 2 # 10yr of semi forward rate 

solve_for_cap <- function(time){
  a = round(365/2) # actual day count
  K = strike[1,which(strike[2,]==time)] 
  t = seq(0.5,time,by = 0.5)[2:(2*time)] # convert into year
  index = seq(1,length(t),1)
  tempt_for = forwar_rate[index]
  tempt_sigma = sigma[index]
  tempt_Dt = pfilea[index]
  
  d = (log(tempt_for/K)+tempt_sigma^2*(t-0.5)/2)/sqrt(tempt_sigma^2*(t-0.5))
  price  = tempt_Dt * a/360 * (tempt_for * pnorm(d) - K * pnorm(d-sqrt(tempt_sigma^2*(t-0.5))))
  return(sum(price))
}

price <- matrix(sapply(c(2:5,7,10),solve_for_cap),nrow=1)
