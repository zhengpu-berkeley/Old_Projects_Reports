
f2 <- function(xk, d, target_density) {
  
  h = function(target_density){ log(target_density(x, ...)) }
  l = length(xk)
  z = numeric(l+1)
  z[1] = d[1]
  z[l+1] = d[2]
  
  # for-loop function for z
  
  #for (i in 1:(l-1)) {
  #  
  #  z[i+1]=  (h(xk[i+1])- h(xk[i])- xk[i+1]*numderiv(h,xk[i+1])+xk[i]*numderiv(h,xk[i]))/(numderiv(xk[i])-numderiv(xk[i+1]))
  #                                                                                        
  #}
   
  # vectorized function for z
  z[2:l] = (h(xk[2:l]) - h(xk[1:(l-1)]) - xk[2:l] * 
            numderiv(h, xk[2:l]) + xk[1:(l-1)] * numderiv(h, xk[1:(l-1)])) /
           (numderiv(xk[1:(l-1)]) - numderiv(xk[2:l]))
  
  s=0
  I=numeric(l)
  
  for (j in 1:l) {
    
    integ= exp(h(xk[j]))/numderiv(h,xk[j])*(exp((z[j]-xk[j])*numderiv(h,xk[j]))-exp((z[j-1]-xk[j])*numderiv(h,xk[j])))
    
    s=s+integ
    
    I[j]=s
    
  }
  
  I=I/I[l]
  
}