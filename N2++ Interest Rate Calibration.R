library(ggplot2)
library(readxl)

df <- read_excel("C:/Users/mulin/Desktop/MFE Spring 2019/Fixed Income/HW5/Homework_5.xlsx")
df[, c(5:10)] = df[, c(5:10)]/100 # convert % to decimal
n = length(df$cmt0.25) # 650 weeks


# close form solution for Vasicek model
AT <- function(sigma, beta, alpha, t){
  AT = (sigma^2/(2*beta^2)-alpha/beta)*t+(alpha/(beta^2)-sigma^2/beta^3)*
             (1-exp(-beta*t))+sigma^2/(4*beta^3)*(1-exp(-2*beta*t))
  return(AT)
}

BT <- function(beta, t){
  BT = 1/beta*(1-exp(-beta*t))
  return(BT)
}

YTM = function(ATx,BTx,ATy,BTy,t,x,y){
  return(-ATx/t + BTx/t*x - ATx/t + BTy/t * y)
}

# N2++ model with parameters that has RMSE
fn = function(parameters){
  alphax = parameters[1]
  sigmax = parameters[2]
  sigmay = parameters[3]
  betax = parameters[4]
  betay = parameters[5]
  periods = c(0.25, 2,3,5,7,10) # = "cmt" per df
  
  for (i in periods){
    assign(paste("Ax", i, sep = ""), AT(sigmax, betax, alphax, i))
    assign(paste("Ay", i, sep = ""), AT(sigmay, betay, 0, i)) # alphay=0
    assign(paste("Bx", i, sep = ""), BT(betax, i))
    assign(paste("By", i, sep = ""), BT(betay, i))
  }

  XY = matrix(0, nrow = n, ncol = 2)
  
  #assume cmt.25 nad cmt10 fit the model perfectly
  for (i in 1:n){
    mat = matrix(c(Bx0.25/0.25, Bx10/10, By0.25/0.25, By10/10), nrow = 2)
    b = c(df$cmt0.25[i]+ Ax0.25/0.25 + Ay0.25/0.25, df$cmt10[i]+ Ax10/10 + Ay10/10)
    XY[i,] = solve(mat,b)
  }
  ytm2 = YTM(Ax2,Bx2,Ay2,By2,2, XY[,1], XY[,2])
  ytm3 = YTM(Ax3,Bx3,Ay3,By3,3, XY[,1], XY[,2])
  ytm5 = YTM(Ax5,Bx5,Ay5,By5,5, XY[,1], XY[,2])
  ytm7 = YTM(Ax7,Bx7,Ay7,By7,7, XY[,1], XY[,2])
  RMSE = sqrt((sum((ytm2 - df$cmt2)^2) + sum((ytm3 - df$cmt3)^2) + sum((ytm5 - df$cmt5)^2) + sum((ytm7 - df$cmt7)^2))/(4*n))
  
  return(RMSE)
}

out = optim(c(0.1,0.2,0.2,0.4,-0.1), fn)

alphax = out$par[1]
sigmax = out$par[2]
sigmay = out$par[3]
betax = out$par[4]
betay = out$par[5]

parameter <- matrix(c(alphax, sigmax, sigmay,betax, betay),nrow=1)
colnames(parameter) <- c("alpha_x", "sigma_x", "sigma_y","beta_x", "beta_y")
print(parameter)



## Actual VS Model
# same copy from the fn function
periods = c(0.25, 2,3,5,7,10) # = "cmt" per df

# plug in alpha, beta and sigma into AT and BT
for (i in periods){
  assign(paste("Ax", i, sep = ""), AT(sigmax, betax, alphax, i))
  assign(paste("Ay", i, sep = ""), AT(sigmay, betay, 0, i)) # alphay=0
  assign(paste("Bx", i, sep = ""), BT(betax, i))
  assign(paste("By", i, sep = ""), BT(betay, i))
}
XY = matrix(0, nrow = n, ncol = 2)
  
#assume cmt.25 nad cmt10 fit the model perfectly
for (i in 1:n){
  mat = matrix(c(Bx0.25/0.25, Bx10/10, By0.25/0.25, By10/10), nrow = 2)
  b = c(df$cmt0.25[i]+ Ax0.25/0.25 + Ay0.25/0.25, df$cmt10[i]+ Ax10/10 + Ay10/10)
  XY[i,] = solve(mat,b)
}

ytm0.25 = YTM(Ax0.25, Bx0.25, Ay0.25, By0.25, 0.25, XY[,1], XY[,2])
ytm2 = YTM(Ax2,Bx2,Ay2,By2,2, XY[,1], XY[,2])
ytm3 = YTM(Ax3,Bx3,Ay3,By3,3, XY[,1], XY[,2])
ytm5 = YTM(Ax5,Bx5,Ay5,By5,5, XY[,1], XY[,2])
ytm7 = YTM(Ax7,Bx7,Ay7,By7,7, XY[,1], XY[,2])
ytm10 = YTM(Ax10,Bx10,Ay10,By10,10, XY[,1], XY[,2])

XY = data.frame(XY)
colnames(XY) = c('X','Y')
