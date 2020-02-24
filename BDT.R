library(readxl)

DT <- function(r_star,rt,sigma,n){
  Dt = rep(1,n+1)
  if (n == 1){
    return (1/(1+r_star/2))
  } else {
    for (i in n:1){
      if (i == n){
        # by the relation that r_ud = r_uu * exp(-2*sigma[i-1] * sqrt(0.5))
        r_i = r_star*exp(-2*sigma[i-1]*sqrt(0.5)*(0:(i-1)))
      } else {
        r_i = rt[1:i,i]
      }
      Dt[1:i] = 0.5*(Dt[1:i]+Dt[2:(i+1)])/(1+r_i/2)
    }
    return(Dt[1])
  }
}


solve_r_stat <- function(Dt_i,rt,sigma,n){
  a = 0
  b = 1
  r_star = (a+b)/2
  # control for fitness to the observation
  error = (DT(r_star,rt,sigma,n)-Dt_i)/Dt_i
  while (abs(error)>0.0001){
    if(error > 0){
      a = r_star
      r_star = (a+b)/2
    } else {
      b = r_star
      r_star = (a+b)/2
    }
    error = (DT(r_star,rt,sigma,n)-Dt_i)/Dt_i
  }
  return(r_star)
}

sigma = voldat$X1
Dt = pfilea$X1
n = 30
rt = matrix(rep(0,900),ncol = n)
rt[1,1] = (1/Dt[1]-1)*2

for (i in 1:n){
  r_star_i = solve_r_stat(Dt[i],rt,sigma,i)
  if (i == 1){
    rt[1:i,i] = r_star_i # assign first value
  }else {
    # by the relation that r_ud = r_uu * exp(-2*sigma[i-1] * sqrt(0.5))
    rt[1:i,i] = r_star_i * exp(-2*sigma[i-1]*sqrt(0.5)*(0:(i-1))) 
  }
}
